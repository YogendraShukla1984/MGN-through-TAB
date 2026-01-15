# Functional Specification: Main Gate Entry (MGN) Enhancement

## Overview
This document describes the functional specification for enhancing the Main Gate Entry (MGN) save process with conditional deletion logic based on configuration parameters.

---

## Business Process Flow

### 1. Main Gate Entry Save Process

When a Main Gate Entry (MGN) is saved, the following function module is triggered:

**Function Module:** `/RWHM/ZMAIN_GATE_ENTRY`

---

## Technical Implementation

### Function Module Call Hierarchy

```
/RWHM/ZMAIN_GATE_ENTRY (Main Entry Point)
    ├── Z_SCM_YTTS_FUNCTION_CHK (Status Validation)
    │   └── Z_LOG_YTTS_FUNC_LIST (Function List Retrieval)
    └── Enhanced Logic (New Implementation)
```

---

### Detailed Function Module Flow

#### 1. `/RWHM/ZMAIN_GATE_ENTRY` - Main Gate Entry Save

**Purpose:** Main function module called during MGN save operation

**Calls:** `Z_SCM_YTTS_FUNCTION_CHK`

---

#### 2. `Z_SCM_YTTS_FUNCTION_CHK` - Function Status Check

**Purpose:** Validate current and next status (Function) of the reporting number

**Process:**
- Checks the current status (Function) of the reporting number
- Determines the next valid status (Function) based on business rules

**Calls:** `Z_LOG_YTTS_FUNC_LIST`

---

#### 3. `Z_LOG_YTTS_FUNC_LIST` - Function List Retrieval

**Purpose:** Retrieve the list of applicable functions (statuses) for the entered truck number

**Process Steps:**

##### Step i) Get Function List
Retrieve the list of functions (statuses) applicable for the entered truck number based on:
- **I_TRUCKNO** - Input truck number
- **I_UNAME** - User name

##### Step ii) Fetch YTTS Data
After getting the function list, fetch data from **YTTS** table using the following selection criteria:

| Field | Description |
|-------|-------------|
| **area** | Area |
| **transplpt** | Transportation Point |
| **vstel** | Shipping Point |
| **trk_purpos** | Truck Purpose |
| **matgr** | Material Group |

**SQL Query Example:**
```abap
SELECT *
  FROM ytts
  INTO TABLE lt_ytts
  WHERE area       = lw_ytts1-area
    AND transplpt  = lw_ytts1-transplpt
    AND vstel      = lw_ytts1-vstel
    AND trk_purpos = lw_ytts1-trk_purpos
    AND matgr      = lw_ytts1-matgr.
```

##### Step iii) Delete Initial Truck Order Records
Delete records from internal table `lt_ytts` where `trk_order` is initial (blank).

**Existing Logic:**
```abap
DELETE lt_ytts WHERE trk_order IS INITIAL.
```

##### Step iv) **New Enhancement - Conditional Delete Logic**

**Requirement:** The DELETE statement (Step iii) should be executed conditionally based on a configuration parameter.

**Implementation:**

**Configuration Table:** `ZLOG_EXEC_VAR`

**Parameter Name:** `ZSCM_MOB_MGN_DEL_ACT`

**Logic:**
- Check if the parameter `ZSCM_MOB_MGN_DEL_ACT` is **Active** in table `ZLOG_EXEC_VAR`
- **IF Active:** Execute the DELETE statement to remove records where `trk_order` is initial
- **IF NOT Active:** Skip the DELETE statement and proceed with all records as-is

---

## Enhanced Logic - Pseudo Code

```abap
* Step 1: Get applicable functions for truck number
CALL FUNCTION 'Z_LOG_YTTS_FUNC_LIST'
  EXPORTING
    i_truckno    = lv_truck_no
    i_uname      = sy-uname
  IMPORTING
    et_func_list = lt_func_list.

* Step 2: Fetch YTTS data based on criteria
SELECT *
  FROM ytts
  INTO TABLE lt_ytts
  WHERE area       = lw_ytts1-area
    AND transplpt  = lw_ytts1-transplpt
    AND vstel      = lw_ytts1-vstel
    AND trk_purpos = lw_ytts1-trk_purpos
    AND matgr      = lw_ytts1-matgr.

* Step 3: Check if delete parameter is active
SELECT SINGLE active
  FROM zlog_exec_var
  INTO @DATA(lv_delete_active)
  WHERE name = 'ZSCM_MOB_MGN_DEL_ACT'
    AND active = 'X'.

* Step 4: Conditionally delete records based on parameter
IF sy-subrc = 0 AND lv_delete_active = 'X'.
  " Parameter is active - delete records with initial truck order
  DELETE lt_ytts WHERE trk_order IS INITIAL.
ELSE.
  " Parameter is not active - proceed with all records
  " No deletion performed
ENDIF.

* Step 5: Continue with remaining processing
```

---

## Configuration Requirements

### Table: ZLOG_EXEC_VAR

**Entry Required:**

| Field | Value | Description |
|-------|-------|-------------|
| **NAME** | ZSCM_MOB_MGN_DEL_ACT | Parameter name to control delete action |
| **ACTIVE** | X or blank | X = Active (delete enabled), Blank = Inactive (delete disabled) |

---

## Impact Analysis

### Systems Affected
- Main Gate Entry (MGN) process
- Truck Tracking System (YTTS)
- Mobile application for MGN

### Tables Affected
- **YTTS** - Truck Tracking System Master
- **ZLOG_EXEC_VAR** - Execution Variables Configuration

### Function Modules Modified
- **Z_LOG_YTTS_FUNC_LIST** - Add conditional delete logic

---

## Testing Scenarios

### Test Case 1: Parameter Active (Delete Enabled)

**Precondition:**
- Parameter `ZSCM_MOB_MGN_DEL_ACT` is set to Active ('X') in `ZLOG_EXEC_VAR`

**Test Steps:**
1. Enter truck number in MGN
2. Save the entry
3. Verify that records with initial `trk_order` are deleted from processing

**Expected Result:**
- Records with blank `trk_order` should NOT appear in function list
- Only records with valid `trk_order` should be processed

---

### Test Case 2: Parameter Inactive (Delete Disabled)

**Precondition:**
- Parameter `ZSCM_MOB_MGN_DEL_ACT` is NOT active (blank or not found) in `ZLOG_EXEC_VAR`

**Test Steps:**
1. Enter truck number in MGN
2. Save the entry
3. Verify that all records are processed, including those with initial `trk_order`

**Expected Result:**
- All records should appear in function list
- No deletion based on `trk_order` should occur

---

### Test Case 3: Parameter Not Found

**Precondition:**
- Parameter `ZSCM_MOB_MGN_DEL_ACT` does not exist in `ZLOG_EXEC_VAR`

**Test Steps:**
1. Enter truck number in MGN
2. Save the entry
3. Verify system behavior

**Expected Result:**
- System should proceed without deletion (same as inactive)
- No error should occur
- All records should be processed

---

## Benefits

1. **Flexibility:** Allows business to enable/disable the delete logic without code changes
2. **Configuration-Driven:** Controlled through configuration table
3. **Backward Compatibility:** Existing functionality remains intact when parameter is inactive
4. **Easy Maintenance:** Can be changed in production without transport
5. **Testing Support:** Different behavior can be tested in different systems

---

## Dependencies

### Upstream Dependencies
- Main Gate Entry transaction/program
- Function module `/RWHM/ZMAIN_GATE_ENTRY`
- Function module `Z_SCM_YTTS_FUNCTION_CHK`

### Downstream Dependencies
- Function module `Z_LOG_YTTS_FUNC_LIST`
- Table `YTTS` - Truck Tracking System
- Table `ZLOG_EXEC_VAR` - Configuration table

---

## Risks & Mitigation

| Risk | Impact | Mitigation |
|------|--------|------------|
| Parameter incorrectly configured | Wrong records filtered | Proper documentation and training |
| Performance impact with large datasets | Slower processing | Additional SELECT check is minimal overhead |
| Missing parameter entry | Unexpected behavior | Default to safe behavior (no deletion) |

---

## Recommendations

1. **Create parameter entry** in all systems (DEV, QAS, PRD) before go-live
2. **Document** the parameter purpose in system configuration documentation
3. **Train** functional team on when to activate/deactivate the parameter
4. **Monitor** after go-live to ensure expected behavior
5. **Add logging** to track when deletion occurs for audit purposes

---

## Appendix

### Related Objects

| Object Type | Object Name | Description |
|-------------|-------------|-------------|
| Function Module | /RWHM/ZMAIN_GATE_ENTRY | Main gate entry save |
| Function Module | Z_SCM_YTTS_FUNCTION_CHK | Status validation |
| Function Module | Z_LOG_YTTS_FUNC_LIST | Function list retrieval |
| Table | YTTS | Truck tracking system master |
| Table | ZLOG_EXEC_VAR | Execution variables configuration |
| Parameter | ZSCM_MOB_MGN_DEL_ACT | Delete action control parameter |

---

## Document Control

| Version | Date | Author | Change Description |
|---------|------|--------|-------------------|
| 1.0 | TBD | TBD | Initial functional specification |

---

**End of Functional Specification**
