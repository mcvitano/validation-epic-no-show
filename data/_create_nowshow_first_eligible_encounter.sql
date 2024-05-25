/*

_create_table_noshow_first_eligible_encounter.sql

SQL:    duckdb
Author: MCvitano01@jpshealth.org
Date:   2024-04-23


Eligibility Criteria:
  - ...
     

*/

with 

eligible_encounters as (

  select
    pat_id,
    pat_enc_csn_id,
    bmi_value as bmi,
    coalesce(recent_noshow_count, 0) as recent_noshow_count,
    cast(appt_made_date as date) as appt_made_date,
    appt_time,

    lower(enc_type) as enc_type,
    lower(visit_type) as visit_type,
    lower(product_type) as product_type,
    lower(smoking_tobacco_use) as smoking_tobacco_use,
    lower(department_name) as department_name,
    lower(appt_status) as appt_status,
    
    -- Version 1 of the Epic No-Show model considers 'late cancels' to
    --  be equivalent to 'no shows'
    case
      when appt_status = 'No Show'
        then 1
      else 0
    end as noshow_flag,

    -- Transform probabilities to [0.001 - 0.999]
    -- Must be able to reconstruct the linear predictor
    -- `  as xb = log(p / (1-p)) ... so p cannot be zero (0)
    case
      when last_predicted_ns = 0
        then 0.001
        
      when last_predicted_ns = 1
        then 0.999
        
      else last_predicted_ns/100
    end as noshow_risk,
    
    case
      when last_predicted_ns < 15
        then '< 0.15'
        
      when last_predicted_ns < 30
        then '< 0.30'
        
      when last_predicted_ns < 40
        then '< 0.40'
        
      else '>= 0.40'
    end as noshow_risk_cat,
      
    case
      when product_type in ('COMMERCIAL', 'NON-CONTRACTED COMMERCIAL')
        then 'commercial'
        
      when product_type in ('MEDICARE', 'MANAGED MEDICARE')
        then 'medicare'
        
      when product_type in ('MEDICAID', 'MANAGED MEDICAID')
        then 'medicaid'
        
      when product_type in ('CHARITY')
        then 'hospital-based medical assistance'
        
      when product_type in ('GOVERNMENT OTHER')
        then 'other'
        
      when product_type in ('TARRANT COUNTY INMATE')
        then 'inmate'
        
      else 'self-pay or liability'
    end as insurance,
    
    case
      when smoking_tobacco_use in (
          'Some Days',
          'Every Day',
          'Light Smoker',
          'Heavy Smoker'
        ) then  'y'
      
      when smoking_tobacco_use in (
          'Never', 'Former'
        ) then 'n'
      
      else 'unknown'
    end as current_smoker_yn,

    -- Need the row-number to facilitate keeping only
    --  patients' *first* eligible encounter
    --  requires ordering with multiple fields to avoid
    --  a non-deterministic operation (i.e., can be multiple appointments
    --  made on the same appt_made_date)
    row_number() over (
      partition by pat_id
      order by appt_made_date, pat_enc_csn_id, appt_time) as encounter_rownum
    
  from encounters
    
  where 1=1
    and enc_type not ilike 'erroneous'
    -- Exclude 'same day' appointments
    -- (1) Date == Scheduled date (regardless of what the Visit Type indicates)
    -- (2) Visit Type == 'Same Day'
    -- 
    -- The VISIT_TYPE field may indicate 'Same Day' but often the appointment
    --  was scheduled to occur later in time).
    -- For example:
    --   CSN: 102429500
    --   Patient scheduled a 'Same Day' appointment on 06-17 (from chart review)
    --     though our data indicates it was scheduled for 06-18 5:40 PM
    and appt_made_date <> cast(appt_time as date)
    and visit_type <> 'SAME DAY'
    -- Exclude appointments occurring prior to day at which they were scheduled
    --  ! Need to ask why this occurs !
    and appt_made_date < cast(appt_time as date)
    -- Face-to-face encounters only (as per EMR Reporting definition)
    and enc_type in (
      'Initial consult',
      'Anti-coag visit',
      'Procedure visit',
      'Office Visit',
      'Routine Prenatal',
      'Initial Prenatal',
      'Postpartum Visit',
      --'Walk-In',           Excluded by Epic's No-show model (suggested)
      'Nurse Only',
      'Social Work',
      'Surgical Consult',
      'Clinical Support',
      'Pre-OP Evaluation',
      'Hospital Encounter',
      'Appointment',
      'TH-Phone',
      'TH-Video',
      'Telemedicine'
      --'SBIRT'              JPS-site specific
    )
    
),

homeless_past_180_days as (

  select
    pat_id,
    max(documentation_date) as recent_homelessness_date
    
  from homelessness
  
    left join encounters
      using (pat_id)
      
  where
    date_diff('day', documentation_date, appt_made_date) >= 0
    and date_diff('day', documentation_date, appt_made_date) <= 180
    
  group by pat_id

),

demographics as (

  select
    pat_id,
    cast(birth_date as date) as birth_date,

    lower(coalesce(pat_sex, 'unknown')) as sex,
    lower(coalesce(is_adopted_yn, 'unknown')) as is_adopted_yn,

    case
        -- original value is 'Hispanic, Latino or Spanish ethnicity'
        when ethnic_group_nm = 'Hispanic, Latino or Spanish ethnicity'
            then 'hispanic'
        when patient_race ilike '%black%'
            then 'nh black'
        when
            patient_race ilike 'asian%'
            or patient_race ilike '%indian%'
            or patient_race ilike '%hawaiian%'
            or patient_race ilike '%other%'
            then 'nh other'
        when patient_race ilike '%caucasian%'
            then 'nh white'
        else 'unknown'
    end as raceth,

    case
        when language_nm = 'English'
            then 'english'
        when language_nm = 'Spanish'
            then 'spanish'
        when
            language_nm in (
                'Deaf (none ASL)',
                'American Sign Language'
            )
            then 'american sign language'
        when
            language_nm = 'Unknown'
            or language_nm is null
            then 'unknown'
        else 'other'
    end as language_primary,

    case
        when
            marital_status_nm in (
                'Divorced',
                'Legally Separated',
                'Single',
                'Widowed'
            )
            then 'single'
        when
            marital_status_nm in (
                'Common Law',
                'Life Partner',
                'Married',
                'Significant Other'
            )
            then 'in a relationship'
        when marital_status_nm = 'Other'
            then 'other'
        else 'unknown'
    end as relationship_status
    
  from patients
  
)

select
  pat_id,
  -- Encounter-level info
  pat_enc_csn_id,
  enc_type,
  visit_type,
  product_type,
  insurance,
  smoking_tobacco_use,
  current_smoker_yn,
  bmi,
  recent_noshow_count,
  appt_made_date,
  appt_time,
  department_name,
  noshow_risk,
  noshow_risk_cat,
  appt_status,
  noshow_flag,
  -- Patient-level info
  birth_date,
  sex,
  raceth,
  is_adopted_yn,
  language_primary,
  relationship_status,
  -- Homelessness
  recent_homelessness_date,
  
  case
    when recent_homelessness_date is not NULL
      then 1
    else 0
  end as homeless_180_days_flag,
  
  date_diff('year', birth_date, appt_made_date) as age,
  
    -- Re-construct the linear predictor from the model predictions
  log(noshow_risk / (1 - noshow_risk)) as linear_predictor

from eligible_encounters
  
  left join demographics
    using (pat_id)
    
  left join homeless_past_180_days
    using (pat_id)

where 1=1
  -- first eligible encounter
  and encounter_rownum = 1
  -- adults only
  and date_diff('year', birth_date, appt_made_date) >= 18
