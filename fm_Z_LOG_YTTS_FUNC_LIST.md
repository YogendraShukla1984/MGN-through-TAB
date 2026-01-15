# Function Module: Z_LOG_YTTS_FUNC_LIST

## Overview
Get Function List based on Truck Number

## Technical Details

| Property | Value |
|----------|-------|
| **Title** | Get Function List |
| **Create Date** | 06.09.2016 |
| **Release** | 6.0 |
| **Technical Author** | Ramesh Manka |
| **Functional Author** | Preyas Desai |

## Interface

### Importing Parameters

| Parameter | Type | Optional | Description |
|-----------|------|----------|-------------|
| I_TRUCKNO | YTRUCK_NO | Yes | Truck Number |
| I_UNAME | UNAME | Yes | User Name |

### Exporting Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| E_AREA | YAREA | Area |
| E_AREADESC | VAL_TEXT | Area Description |
| E_REPORTNO | YREPORT_NO | Report Number |
| E_MATCODE | MAKTX | Material Description |
| E_INVOICE | VBELN_VF | Invoice Number |
| ET_FUNC_LIST | ZTT_FUNC_LIST | Function List Table |
| ET_RETURN | BAPIRET2_T | Return Messages |
| E_LICNO | YLICNO | License Number |
| E_DRIVER | YDRIVER | Driver |
| E_MNAME | ZMNAME | Middle Name |
| E_LNAME | ZNAME3 | Last Name |

## Change History

| Sr No | Date | User ID | Description | Change Label | Report Output |
|-------|------|---------|-------------|---------------|---------------|
| 01 | 08.02.18 | pwc_159 | Jobwork scenario | Functional: Bangarraju | |
| 02 | 30.08.2018 | Peol-007 | Sort Statement change only | Manoj Swami 8025018 | |
| 03 | 07.09.2018 | brilo_005 | Logic change to get the latest record based on MG entered & PP entered date | Manoj Swami 8025018 | |

## Data Types

### Local Type Definitions

#### lty_ytts1
```abap
BEGIN OF lty_ytts1,
  area          TYPE yarea,
  report_no     TYPE yreport_no,
  truck_no      TYPE ytruck_no,
  transplpt     TYPE tplst,
  reject_res    TYPE yreasoncd,
  trk_purpos    TYPE ytrk_purps,
  vstel         TYPE vstel,
  matgr         TYPE mvgr1,
  mg_entr_dt    TYPE datum,
  mg_entr_tm    TYPE uzeit,
  pp_entr_dt    TYPE datum,
  pp_entr_tm    TYPE uzeit,
  mg_exit_dt    TYPE datum,
  mg_exit_tm    TYPE uzeit,
  function      TYPE ystats,
  licno         TYPE ylicno,
  driver        TYPE ydriver,
  mname         TYPE zmname,
  lname         TYPE zname3,
  editdt        TYPE dats,
  edittm        TYPE uzeit,
  wfinal        TYPE zfeld,
  rej_res_ar    TYPE ystatusflg,
END OF lty_ytts1
```

#### lty_yttstx0005
```abap
BEGIN OF lty_yttstx0005,
  area          TYPE yarea,
  function      TYPE ystats,
  descr         TYPE ydescr,
END OF lty_yttstx0005
```

#### lty_ytts2
```abap
BEGIN OF lty_ytts2,
  area          TYPE yarea,
  report_no     TYPE yreport_no,
  mat_code      TYPE matnr,
  maktx         TYPE maktx,
  billno        TYPE vbeln_vf,
END OF lty_ytts2
```

#### lty_tpfn
```abap
BEGIN OF lty_tpfn,
  fn   TYPE char4,
  seq  TYPE char1,
END OF lty_tpfn
```

#### lty_ztcfunction
```abap
BEGIN OF lty_ztcfunction,
  tcfunct TYPE ztcfunct,
  tcdesc  TYPE ztcdesc,
  yttsmap TYPE zytts_tc_map,
END OF lty_ztcfunction
```

#### lty_exec_var
```abap
BEGIN OF lty_exec_var,
  name     TYPE rvari_vnam,
  function TYPE ystats,
  matgr    TYPE zlog_exec_var-matgr,
  area     TYPE zlog_exec_var-area,
END OF lty_exec_var
```

#### lty_ztcbpact
```abap
BEGIN OF lty_ztcbpact,
  area     TYPE ztcbpact-area,
  tcfunct  TYPE ztcbpact-tcfunct,
  matgr    TYPE ztcbpact-matgr,
  tccode   TYPE ztcbpact-tccode,
  datefrom TYPE ztcbpact-datefrom,
  timefrom TYPE ztcbpact-timefrom,
  dateto   TYPE ztcbpact-dateto,
  timeto   TYPE ztcbpact-timeto,
END OF lty_ztcbpact
```

#### lty_exec
```abap
BEGIN OF lty_exec,
  name TYPE rvari_vnam,
  numb TYPE tvarv_numb,
  area TYPE yarea,
END OF lty_exec
```

#### lty_param_ytts
```abap
BEGIN OF lty_param_ytts,
  name      TYPE zlog_exec_var-name,
  active    TYPE zlog_exec_var-active,
  remarks   TYPE zlog_exec_var-remarks,
  area      TYPE zlog_exec_var-area,
  function  TYPE zlog_exec_var-function,
END OF lty_param_ytts
```

#### lty_func_list
```abap
BEGIN OF lty_func_list,
  function TYPE  ystats,
  date     TYPE dats,
  time     TYPE time,
  descr    TYPE ydescr,
  ind      TYPE char2,
  flag     TYPE char1,
END OF lty_func_list
```

#### lty_zpara_jw
```abap
BEGIN OF lty_zpara_jw,
  param1  TYPE zyttspara-param1,
  param2  TYPE zyttspara-param2,
  value1  TYPE zyttspara-value1,
  value2  TYPE zyttspara-value2,
END OF lty_zpara_jw
```

## Constants

| Constant | Type | Value | Description |
|----------|------|-------|-------------|
| lc_yttsapp | zyttspara-param1 | 'YTTSAPP' | YTTS Application |
| lc_jobwork | zyttspara-param2 | 'YTTS_JOBWORK' | YTTS Jobwork |
| gc_x | c | 'X' | Flag |
| gc_name | rvari_vnam | 'AREA_EXCLUDE_TRUCK_F4' | Area Exclude Truck F4 |

## Main Logic Flow

### 1. Initial Validations
- Check if user name (I_UNAME) is provided
- Check if truck number (I_TRUCKNO) is provided
- Return error if truck number is not provided

### 2. Data Retrieval
- Fetch truck tracking data from **YTTSTX0001** table based on truck number
- Delete records with initial truck numbers
- Delete MGX records without entry dates
- Exclude areas based on configuration in **ZLOG_EXEC_VAR** table

### 3. Latest Record Selection
- Logic to get the latest record based on:
  - MG entered date and time
  - PP entered date and time
- Sort by PP entry date/time in descending order

### 4. Master Data Selection
Select data from multiple tables:
- **YTTSA** - Truck Tracking Status Archive
- **YTTS** - Truck Tracking System
- **YTTSTX0005** - Function Descriptions
- **ZTCBPACT** - Trip Check Bypass Activity
- **ZTCFUNCTION** - Trip Check Function
- **ZTCHDR** - Trip Check Header
- **ZLOG_EXEC_VAR** - Execution Variables
- **YTTSTX0002** - Material and Invoice Details
- **MAKT** - Material Description

### 5. Function List Building
Build function list based on:
- Truck status (before/after TPN - Truck Presence Notification)
- Trip check functions and their sequence
- Authority checks for user
- Bypass activities configuration
- Reject indicators

### 6. Authority Checks
Perform authority checks using:
- **YTTS_AUTH** - Authorization Object for YTTS
- **ZLOG_TRPCK** - Authorization Object for Trip Check

### 7. Special Handling

#### Reject Reason Processing
- Check for reject reasons in truck data
- Filter functions based on reject indicator configuration
- Delete non-performed functions if reject reason exists

#### Jobwork Scenario (Commented)
- Historical logic for jobwork scenarios (currently commented)
- Was based on truck purpose and shipping point

### 8. Output Preparation
Export parameters populated:
- Area and Area Description
- Report Number
- Material Description
- Invoice Number
- License Number, Driver details
- Function List with indicators:
  - **Z** - Performed/Completed
  - **A1** - Available (Pay & Park)
  - **A2** - Available (Trip Check)
  - **A3** - Available (Pending Approval)
  - **A4** - Available (Loading)
  - **B** - Available (Bay)
  - **B1** - Available (Pending Status)
  - **D** - Disabled
  - **R** - Rejected

## Function Indicators Description

| Indicator | Description |
|-----------|-------------|
| Z | Function is completed/performed |
| A1 | Available for Pay & Park function |
| A2 | Available for Trip Check function |
| A3 | Available - Pending Approval |
| A4 | Available - Loading function |
| B | Available - Bay function |
| B1 | Available - Pending Status |
| D | Disabled function |
| R | Rejected function |

## Database Tables Used

| Table | Description |
|-------|-------------|
| YTTSTX0001 | Truck Tracking Extended 0001 |
| YTTSTX0002 | Truck Tracking Extended 0002 (Material/Invoice) |
| YTTSTX0005 | Function Descriptions |
| YTTS | Truck Tracking System Master |
| YTTSA | Truck Tracking Status Archive |
| ZTCBPACT | Trip Check Bypass Activity |
| ZTCFUNCTION | Trip Check Function Master |
| ZTCHDR | Trip Check Header |
| ZLOG_EXEC_VAR | Execution Variables Configuration |
| MAKT | Material Description |
| ZYTTSPARA | YTTS Parameters |

## Function Modules Called

| Function Module | Purpose |
|-----------------|---------|
| CONVERSION_EXIT_ALPHA_OUTPUT | Remove leading zeros from numbers |
| DOMAIN_VALUE_GET | Get domain value description |
| AUTHORITY_CHECK | Check user authorization |
| CONVERT_INTO_TIMESTAMP | Convert date and time to timestamp |

## Key Features

1. **Truck Status Tracking**: Maintains complete history of truck movements
2. **Authority-Based Function Display**: Shows only authorized functions to users
3. **Trip Check Integration**: Handles trip check processes with bypass logic
4. **Reject Reason Handling**: Special processing for rejected trucks
5. **Time-Based Function Availability**: Functions available based on date/time windows
6. **Material Group Specific Logic**: Different processing based on material groups
7. **Area-Specific Configuration**: Area-based function availability and sequencing
8. **Pay & Park Support**: Special handling for pay and park functionality

## Notes

- The function uses extensive authorization checks to control user access
- Bypass activities can be configured in ZTCBPACT table
- Function sequence is determined by the YTTS master table configuration
- Historical jobwork scenario logic is currently commented out
- Reject reason processing was added by Eswara on 11.07.2022
- Trip check functions use a specific sequence order
- Date/time stamps are converted for comparison purposes
