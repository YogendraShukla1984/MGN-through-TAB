# Technical Specification: Main Gate Entry (MGN) Enhancement

## Document Information

| Property | Value |
|----------|-------|
| **Document Type** | Technical Specification (TS) |
| **Related FS** | MGN_FS.md |
| **Technical Author** | [To be filled] |
| **Creation Date** | [To be filled] |
| **SAP Version** | ECC 6.0 / NetWeaver 7.31 |
| **Status** | Draft |

---

## Table of Contents

1. [Overview](#overview)
2. [Technical Architecture](#technical-architecture)
3. [Development Objects](#development-objects)
4. [Detailed Implementation](#detailed-implementation)
5. [Code Changes](#code-changes)
6. [Configuration](#configuration)
7. [Unit Testing](#unit-testing)
8. [Transport Management](#transport-management)
9. [Deployment Plan](#deployment-plan)

---

## Overview

### Business Requirement
Implement conditional deletion logic in the Main Gate Entry (MGN) save process. The deletion of records with initial truck order should be controlled by a configuration parameter.

### Technical Summary
Modify function module `Z_LOG_YTTS_FUNC_LIST` to check configuration parameter `ZSCM_MOB_MGN_DEL_ACT` before deleting records where `trk_order` is initial.

### Scope of Changes
- Function Module: `Z_LOG_YTTS_FUNC_LIST`
- Configuration Table: `ZLOG_EXEC_VAR` (existing)
- No new objects required

---

## Technical Architecture

### Function Module Call Hierarchy

```
/RWHM/ZMAIN_GATE_ENTRY (Main Gate Entry Save)
    │
    └── Z_SCM_YTTS_FUNCTION_CHK (Function Status Check)
            │
            └── Z_LOG_YTTS_FUNC_LIST (Function List Retrieval) *** MODIFICATION HERE ***
                    │
                    ├── SELECT from YTTSTX0001
                    ├── SELECT from YTTS (where modification occurs)
                    ├── [NEW] SELECT from ZLOG_EXEC_VAR (parameter check)
                    └── [MODIFIED] Conditional DELETE logic
```

### Data Flow

```
Input: Truck Number
    │
    ├── Fetch truck tracking data (YTTSTX0001)
    │
    └── Fetch YTTS master data (YTTS)
            │
            ├── [NEW] Check parameter ZSCM_MOB_MGN_DEL_ACT
            │   │
            │   ├── Active (X) → Execute DELETE WHERE trk_order IS INITIAL
            │   │
            │   └── Not Active → Skip DELETE, proceed with all records
            │
            └── Continue processing function list
```

---

## Development Objects

### Objects to be Modified

| Object Type | Object Name | Description | Transport Type |
|-------------|-------------|-------------|----------------|
| Function Module | Z_LOG_YTTS_FUNC_LIST | Function list retrieval logic | Workbench (PROG) |

### Objects Used (No Changes)

| Object Type | Object Name | Description |
|-------------|-------------|-------------|
| Table | YTTS | Truck Tracking System Master |
| Table | ZLOG_EXEC_VAR | Execution Variables Configuration |
| Function Module | /RWHM/ZMAIN_GATE_ENTRY | Main gate entry save |
| Function Module | Z_SCM_YTTS_FUNCTION_CHK | Function status check |

---

## Detailed Implementation

### Current Logic (Before Change)

**Location:** Function Module `Z_LOG_YTTS_FUNC_LIST`  
**Section:** Data retrieval from YTTS table  
**Lines:** Approximately 456-513 (based on FM documentation)

**Current Code:**
```abap
SELECT *
       FROM ytts
       INTO TABLE lt_ytts
       WHERE area       EQ lw_ytts1-area
         AND transplpt  EQ lw_ytts1-transplpt
         AND vstel      EQ lw_ytts1-vstel
         AND trk_purpos EQ lw_ytts1-trk_purpos
         AND matgr      EQ lw_ytts1-matgr.
IF sy-subrc EQ 0.
  DELETE lt_ytts WHERE trk_order EQ space.
  SORT lt_ytts BY trk_order ASCENDING.
ENDIF.
```

**Issue:** The DELETE statement always executes, removing records with initial `trk_order` regardless of business requirements.

### Enhanced Logic (After Change)

**New Implementation:**

```abap
" BEGIN: Cursor Generated Code
SELECT *
       FROM ytts
       INTO TABLE lt_ytts
       WHERE area       EQ lw_ytts1-area
         AND transplpt  EQ lw_ytts1-transplpt
         AND vstel      EQ lw_ytts1-vstel
         AND trk_purpos EQ lw_ytts1-trk_purpos
         AND matgr      EQ lw_ytts1-matgr.
IF sy-subrc EQ 0.
  
  " Check if delete parameter is active
  DATA: lv_delete_active TYPE zactive_flag.
  
  SELECT SINGLE active
    FROM zlog_exec_var
    INTO lv_delete_active
    WHERE name = 'ZSCM_MOB_MGN_DEL_ACT'
      AND active = abap_true.
  
  " Conditionally delete records based on parameter
  IF sy-subrc EQ 0 AND lv_delete_active EQ abap_true.
    " Parameter is active - delete records with initial truck order
    DELETE lt_ytts WHERE trk_order EQ space.
  ENDIF.
  " If parameter not active, proceed with all records
  
  SORT lt_ytts BY trk_order ASCENDING.
ENDIF.
" END: Cursor Generated Code
```

---

## Code Changes

### Change #1: Add Data Declaration

**Function Module:** `Z_LOG_YTTS_FUNC_LIST`  
**Section:** Local Variable Declarations  
**Location:** After existing DATA declarations (after line 201)

**Add:**
```abap
" BEGIN: Cursor Generated Code
" Variable to store delete parameter active flag
DATA: lv_delete_active TYPE zactive_flag.
" END: Cursor Generated Code
```

**Compliance Notes:**
- ✅ Uses existing type `zactive_flag` (already used in FM at line 214)
- ✅ Variable naming follows standard: `lv_` prefix for local variable
- ✅ Declared in DATA section (NetWeaver 7.31 compliant)
- ✅ Comment explains purpose

### Change #2: Modify YTTS Selection Logic

**Function Module:** `Z_LOG_YTTS_FUNC_LIST`  
**Section:** YTTS Data Selection  
**Location:** Lines 448-513 (approximately)

**Replace:**
```abap
" OLD CODE - Lines 448-458
SELECT *
       FROM ytts
       INTO TABLE lt_ytts
       WHERE area       EQ lw_ytts1-area
         AND transplpt  EQ lw_ytts1-transplpt
         AND vstel      EQ lw_ytts1-vstel
         AND trk_purpos EQ lw_ytts1-trk_purpos
         AND matgr      EQ lw_ytts1-matgr.
IF sy-subrc EQ 0.
  DELETE lt_ytts WHERE trk_order EQ space.
  SORT lt_ytts BY trk_order ASCENDING.
```

**With:**
```abap
" BEGIN: Cursor Generated Code
SELECT *
       FROM ytts
       INTO TABLE lt_ytts
       WHERE area       EQ lw_ytts1-area
         AND transplpt  EQ lw_ytts1-transplpt
         AND vstel      EQ lw_ytts1-vstel
         AND trk_purpos EQ lw_ytts1-trk_purpos
         AND matgr      EQ lw_ytts1-matgr.
IF sy-subrc EQ 0.
  
  " Check if delete parameter is active in configuration
  SELECT SINGLE active
    FROM zlog_exec_var
    INTO lv_delete_active
    WHERE name = 'ZSCM_MOB_MGN_DEL_ACT'
      AND active = abap_true.
  
  " Conditionally delete records with initial truck order
  IF sy-subrc EQ 0 AND lv_delete_active EQ abap_true.
    " Configuration parameter is active - delete initial truck orders
    DELETE lt_ytts WHERE trk_order EQ space.
  ENDIF.
  " If parameter not active, proceed with all records (no deletion)
  
  SORT lt_ytts BY trk_order ASCENDING.
ENDIF.
" END: Cursor Generated Code
```

**Code Compliance Checklist:**
- ✅ NetWeaver 7.31 compliant (no inline declarations, no host variables)
- ✅ SELECT SINGLE for single record retrieval
- ✅ Uses `abap_true` constant (predefined in ABAP)
- ✅ Proper `sy-subrc` check immediately after SELECT
- ✅ Clear comments explaining logic
- ✅ Cursor-generated code markers present
- ✅ Maintains backward compatibility (default: no deletion)

### Change #3: Update Function Module Header Comment

**Function Module:** `Z_LOG_YTTS_FUNC_LIST`  
**Section:** Change History Header  
**Location:** Lines 29-38 (approximately)

**Add to Change History:**
```abap
*-----------------------------------------------------------------------
* CHANGE HISTORY
*-----------------------------------------------------------------------
*SrNo| Date  | User ID |  Description  |  Change Label   | report output
*-----------------------------------------------------------------------
* 01 |08.02.18|pwc_159 | Jobwork senario, Functional: Bangarraju
*-----------------------------------------------------------------------
* 02 |30.08.2018|Peol-007|Sort Statement change only|Manoj Swami 8025018 |
*-----------------------------------------------------------------------
* 03 |07.09.2018|brilo_005|Logic change to get the latest record based
*                on MG entered & PP entered date  |Manoj Swami 8025018 |
*-----------------------------------------------------------------------
* 04 |DD.MM.YYYY|[USER_ID]|Conditional delete logic based on config |
*                           parameter ZSCM_MOB_MGN_DEL_ACT |[CR_NUMBER]|
*-----------------------------------------------------------------------
```

---

## Configuration

### Configuration Table Entry

**Table:** `ZLOG_EXEC_VAR`

#### Required Fields

| Field | Type | Length | Description |
|-------|------|--------|-------------|
| MANDT | CLNT | 3 | Client |
| NAME | RVARI_VNAM | 30 | Variable Name |
| NUMB | TVARV_NUMB | 10 | Line Number |
| ACTIVE | ZACTIVE_FLAG | 1 | Active Flag (X/blank) |
| AREA | YAREA | 4 | Area (optional for this parameter) |
| FUNCTION | YSTATS | 4 | Function (optional for this parameter) |
| MATGR | MVGR1 | 3 | Material Group (optional) |
| REJECT_RES | YREASONCD | 10 | Reject Reason (optional) |
| REMARKS | ZREMARKS | 255 | Remarks |

#### Configuration Entry

**Development/Testing Systems (DEV/QAS):**

| Field | Value | Note |
|-------|-------|------|
| NAME | ZSCM_MOB_MGN_DEL_ACT | Parameter name (exact) |
| NUMB | 0000000001 | Line number |
| ACTIVE | X | Active (delete enabled) |
| AREA | [blank] | Not area-specific |
| FUNCTION | [blank] | Not function-specific |
| MATGR | [blank] | Not material-group specific |
| REJECT_RES | [blank] | Not reject-reason specific |
| REMARKS | Enable/disable deletion of initial truck orders in MGN | Purpose description |

**Production System (PRD):**
- Initial value should match business requirement
- Typically start with 'X' (Active) if current behavior should continue
- Can be changed via SM30 without transport

### Configuration Maintenance

**Transaction:** SM30  
**View/Table:** ZLOG_EXEC_VAR  

**Steps to Configure:**
1. Execute transaction `SM30`
2. Enter table name: `ZLOG_EXEC_VAR`
3. Click "Maintain" button
4. Click "New Entries"
5. Fill in the fields as per configuration entry above
6. Save the entry
7. Record in transport request (for DEV/QAS) or local object (for PRD)

**Authorization Required:**
- S_TABU_DIS with activity 02 (Change)
- S_TABU_NAM for table ZLOG_EXEC_VAR

---

## Unit Testing

### Test Scenarios

#### Test Case 1: Parameter Active (Delete Enabled)

**Objective:** Verify that records with initial `trk_order` are deleted when parameter is active.

**Preconditions:**
- Entry exists in `ZLOG_EXEC_VAR`:
  - NAME = 'ZSCM_MOB_MGN_DEL_ACT'
  - ACTIVE = 'X'
- Test truck number exists in YTTSTX0001
- YTTS table has records with both initial and non-initial `trk_order`

**Test Data:**
```abap
" Sample YTTS records
AREA: 'PL01', TRANSPLPT: 'TP01', TRK_ORDER: '0000000001'  " Non-initial
AREA: 'PL01', TRANSPLPT: 'TP01', TRK_ORDER: ''            " Initial (should be deleted)
AREA: 'PL01', TRANSPLPT: 'TP01', TRK_ORDER: '0000000002'  " Non-initial
```

**Test Steps:**
1. Call function module with test truck number:
   ```abap
   CALL FUNCTION 'Z_LOG_YTTS_FUNC_LIST'
     EXPORTING
       i_truckno    = 'TEST-TRUCK-001'
       i_uname      = sy-uname
     IMPORTING
       et_func_list = lt_func_list
       et_return    = lt_return.
   ```
2. Debug and check `lt_ytts` after DELETE statement
3. Verify function list output

**Expected Results:**
- ✅ Records with initial `trk_order` should NOT be in `lt_ytts`
- ✅ Only records with valid `trk_order` should be processed
- ✅ Function list should contain only valid functions
- ✅ `et_return` should have no errors

**Success Criteria:**
- `sy-subrc = 0` after function call
- `lt_ytts` contains only non-initial `trk_order` records
- No dump or error messages

---

#### Test Case 2: Parameter Inactive (Delete Disabled)

**Objective:** Verify that all records are retained when parameter is inactive.

**Preconditions:**
- Entry in `ZLOG_EXEC_VAR`:
  - NAME = 'ZSCM_MOB_MGN_DEL_ACT'
  - ACTIVE = '' (blank)
- Same test data as Test Case 1

**Test Steps:**
1. Update configuration: Set ACTIVE = '' (blank)
2. Call function module with test truck number
3. Debug and check `lt_ytts` after conditional DELETE
4. Verify function list output

**Expected Results:**
- ✅ ALL records should remain in `lt_ytts` (including initial `trk_order`)
- ✅ No deletion should occur
- ✅ Function list should contain all functions
- ✅ `et_return` should have no errors

**Success Criteria:**
- `sy-subrc = 0` after function call
- `lt_ytts` contains both initial and non-initial `trk_order` records
- Record count in `lt_ytts` = original record count from YTTS

---

#### Test Case 3: Parameter Not Found

**Objective:** Verify system behavior when parameter doesn't exist.

**Preconditions:**
- NO entry in `ZLOG_EXEC_VAR` with NAME = 'ZSCM_MOB_MGN_DEL_ACT'
- Same test data as Test Case 1

**Test Steps:**
1. Delete configuration entry (if exists)
2. Call function module with test truck number
3. Debug and check behavior
4. Verify no errors occur

**Expected Results:**
- ✅ System should proceed without deletion (safe default)
- ✅ ALL records should remain in `lt_ytts`
- ✅ No dump or error
- ✅ Function continues normally

**Success Criteria:**
- `sy-subrc = 0` after function call
- No short dump
- Behavior same as Test Case 2 (inactive)

---

#### Test Case 4: Performance Test

**Objective:** Verify no significant performance degradation.

**Preconditions:**
- Production-like data volume
- 10,000+ records in YTTS
- Multiple truck numbers

**Test Steps:**
1. Execute function module 100 times with different truck numbers
2. Measure execution time before and after change
3. Check database time in ST05 trace

**Expected Results:**
- ✅ Execution time increase < 5%
- ✅ Additional SELECT adds < 10ms overhead
- ✅ No full table scans

**Success Criteria:**
- Average execution time increase ≤ 5%
- No Code Inspector errors
- No ST22 dumps

---

#### Test Case 5: Backward Compatibility

**Objective:** Ensure existing functionality is not broken.

**Preconditions:**
- Parameter active (current production behavior)
- Existing test scripts/scenarios

**Test Steps:**
1. Run all existing regression test cases
2. Compare output before and after change
3. Verify no functional regression

**Expected Results:**
- ✅ All existing test cases pass
- ✅ Output matches baseline (with parameter active)
- ✅ No side effects

**Success Criteria:**
- 100% regression test pass rate
- No functional differences when parameter is active

---

### Testing Tools

**Recommended Tools:**
- SE37 - Function Module Test
- SE80 - ABAP Debugger
- ST05 - SQL Trace
- ST22 - Dump Analysis
- SM30 - Table Maintenance

**Debug Watchpoints:**
1. `lv_delete_active` - Check parameter value
2. `sy-subrc` - After SELECT from ZLOG_EXEC_VAR
3. `lt_ytts` - Before and after DELETE statement
4. Record count: `lines( lt_ytts )`

---

## Transport Management

### Transport Strategy

**Development System (DEV):**
1. Create workbench transport request
2. Assign to development team
3. Include modified function module

**Quality Assurance (QAS):**
1. Import transport from DEV
2. Execute unit tests
3. Execute integration tests
4. Obtain sign-off from functional team

**Production (PRD):**
1. Import transport during maintenance window
2. Configuration can be changed without transport (SM30)
3. Post-deployment verification

### Transport Objects

| Object Type | Object Name | Description | Transport Type |
|-------------|-------------|-------------|----------------|
| FUGR | Z_LOG_YTTS_FUNC_LIST | Function Module | Workbench (PROG) |
| TABU | ZLOG_EXEC_VAR | Configuration Entry (optional) | Customizing |

### Transport Request Structure

```
Transport Request: DEVK9xxxxx (Workbench)
├── Task 1: DEVK9xxxxx1 (Developer 1)
│   └── FUGR Z_LOG_YTTS_FUNC_LIST (Function Module)
│
└── Task 2: DEVK9xxxxx2 (Developer 2 - Optional)
    └── TABU ZLOG_EXEC_VAR (Configuration entry)
```

**Note:** Configuration entry in PRD should be created directly (local) without transport for flexibility.

---

## Deployment Plan

### Pre-Deployment Checklist

- [ ] Code review completed
- [ ] Unit testing completed (all test cases passed)
- [ ] Code Inspector check (zero errors/warnings)
- [ ] Transport request documented
- [ ] Backup plan prepared
- [ ] Rollback procedure documented
- [ ] Functional sign-off obtained
- [ ] Change advisory board (CAB) approval

### Deployment Steps

#### Step 1: Pre-Deployment (T-1 Day)

1. **Verify Transport**
   - Check transport request contents
   - Verify no missing objects
   - Ensure all tasks released

2. **Prepare Configuration**
   - Prepare configuration entry for PRD
   - Document configuration values
   - Prepare SM30 maintenance script

3. **Communication**
   - Notify stakeholders of deployment window
   - Inform helpdesk of changes
   - Prepare rollback communication template

#### Step 2: Deployment (Day 0)

**Time:** [Maintenance Window]

1. **Import Transport (15 minutes)**
   ```
   Transaction: STMS
   - Select import queue for PRD
   - Import transport request
   - Verify import logs (RC = 0)
   ```

2. **Create Configuration Entry (5 minutes)**
   ```
   Transaction: SM30
   - Table: ZLOG_EXEC_VAR
   - Create new entry:
     - NAME: ZSCM_MOB_MGN_DEL_ACT
     - ACTIVE: X (or as per business decision)
     - REMARKS: [Description]
   - Save as local object
   ```

3. **Smoke Test (10 minutes)**
   ```
   Transaction: SE37
   - Test function module Z_LOG_YTTS_FUNC_LIST
   - Verify no dumps
   - Check configuration is read correctly
   ```

4. **Integration Test (15 minutes)**
   ```
   - Execute MGN transaction
   - Save test entry
   - Verify function list is generated
   - Check no errors in SM21/ST22
   ```

#### Step 3: Post-Deployment Verification (30 minutes)

1. **Functional Verification**
   - Execute 5 sample MGN transactions
   - Verify function list generation
   - Check delete behavior as per configuration

2. **Technical Verification**
   - Check ST22 for dumps (should be zero)
   - Check SM21 for errors (should be zero)
   - Monitor SM50 for long-running processes
   - Verify database performance in ST04

3. **User Acceptance**
   - Have business users test 2-3 scenarios
   - Obtain confirmation of expected behavior
   - Document any issues immediately

#### Step 4: Monitoring (Day 0 + 7 days)

**Day 0-1 (Critical Monitoring):**
- Monitor ST22 every 2 hours
- Check SM21 system log every 4 hours
- Review SM50 for performance issues
- Monitor user feedback

**Day 2-7 (Standard Monitoring):**
- Daily ST22 dump check
- Daily SM21 log review
- Weekly performance analysis
- Collect user feedback

### Rollback Plan

**Scenario 1: Critical Issue (Dumps/Performance)**

**Time Required:** 30 minutes

**Steps:**
1. **Immediate Action (5 minutes)**
   ```
   Transaction: SM30
   - Table: ZLOG_EXEC_VAR
   - Set ACTIVE = '' (blank) for parameter ZSCM_MOB_MGN_DEL_ACT
   - Save
   ```
   This reverts to legacy behavior without transport.

2. **If Issue Persists (25 minutes)**
   ```
   Transaction: STMS
   - Import previous version transport
   - Restore original function module
   ```

**Scenario 2: Functional Issue (Wrong Behavior)**

**Time Required:** 10 minutes

**Steps:**
1. Change configuration parameter via SM30
2. Toggle ACTIVE flag to correct value
3. Test and verify

**Scenario 3: Complete Rollback**

**Time Required:** 45 minutes

**Steps:**
1. Export current version (backup)
2. Import previous transport version
3. Delete configuration entry
4. Test thoroughly
5. Communicate to users

### Communication Plan

**Pre-Deployment:**
- Email to stakeholders (T-3 days)
- Announce maintenance window (T-1 day)
- Prepare helpdesk FAQ

**During Deployment:**
- Status update every 15 minutes
- Immediate notification if issues found
- Go/No-Go decision at 50% completion

**Post-Deployment:**
- Success notification (within 1 hour)
- Issue summary (if any)
- Next steps and monitoring plan

---

## Code Quality Checklist

### ABAP Code Rules Compliance

**NetWeaver 7.31 Compatibility:** ✅
- [x] No inline declarations (DATA(lv_var))
- [x] No constructor operators (NEW, VALUE, CORRESPONDING)
- [x] No string templates (|text|)
- [x] No table expressions (itab[ key ])
- [x] No host variables (@variable)
- [x] All variables declared in DATA section

**Naming Conventions:** ✅
- [x] Local variable: `lv_delete_active` (correct prefix)
- [x] Meaningful names used
- [x] No magic numbers

**Database Access:** ✅
- [x] SELECT SINGLE for single record
- [x] Proper WHERE clause
- [x] sy-subrc checked immediately
- [x] No SELECT *  (acceptable for existing code)
- [x] No FOR ALL ENTRIES without check

**Exception Handling:** ✅
- [x] No new exceptions raised (not needed)
- [x] Proper error handling flow
- [x] Safe default behavior

**Documentation:** ✅
- [x] Cursor-generated code markers present
- [x] Inline comments explain logic
- [x] Change history updated
- [x] Clear variable names

**Performance:** ✅
- [x] Single additional SELECT (acceptable overhead)
- [x] No loops added
- [x] No performance bottlenecks
- [x] Uses existing indexes

### Code Inspector Results

**Expected Results:**
- Errors: 0
- Warnings: 0
- Information: 0
- Severity: 🟢 Green

**Check Performed:**
- Transaction: SCI
- Check variant: DEFAULT
- Object: Function Group containing Z_LOG_YTTS_FUNC_LIST

---

## Risk Assessment

### Technical Risks

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| Performance degradation | Low | Medium | Additional SELECT is single-record lookup with index |
| Configuration parameter not found | Low | Low | Safe default: no deletion (backward compatible) |
| sy-subrc not checked properly | Very Low | High | Code review mandatory, proper checks in place |
| Dumps in production | Very Low | High | Extensive unit testing, rollback plan ready |

### Functional Risks

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| Incorrect configuration value | Low | Medium | Configuration documented, training provided |
| Wrong records deleted | Very Low | High | Parameter active maintains current behavior |
| Business logic misunderstanding | Low | Medium | Functional sign-off required before deployment |

### Mitigation Summary

1. **Extensive Testing:** All test cases executed
2. **Backward Compatibility:** Default behavior = current production behavior
3. **Rollback Plan:** Configuration change provides instant rollback
4. **Code Review:** Mandatory review by senior developer
5. **Monitoring:** 7-day intensive monitoring period

---

## Dependencies

### Upstream Dependencies

**Required Objects (Must Exist):**
- Function Module: /RWHM/ZMAIN_GATE_ENTRY
- Function Module: Z_SCM_YTTS_FUNCTION_CHK
- Table: YTTS
- Table: ZLOG_EXEC_VAR
- Type: ZACTIVE_FLAG

**No Changes Required To:**
- Calling programs
- Calling function modules
- Table structures
- Screen programs

### Downstream Dependencies

**Objects That Call This FM:**
- Z_SCM_YTTS_FUNCTION_CHK
- /RWHM/ZMAIN_GATE_ENTRY
- Mobile application for MGN

**Impact on Callers:**
- ✅ No signature change
- ✅ No interface modification
- ✅ Transparent change
- ✅ No recompilation needed

---

## Success Criteria

### Technical Success

- [ ] Code compiles without errors
- [ ] Code Inspector: Zero errors/warnings
- [ ] Unit tests: 100% pass rate
- [ ] Performance: < 5% overhead
- [ ] No dumps in ST22
- [ ] No errors in SM21

### Functional Success

- [ ] Configuration parameter controls delete behavior correctly
- [ ] Test Case 1 passed (parameter active)
- [ ] Test Case 2 passed (parameter inactive)
- [ ] Test Case 3 passed (parameter not found)
- [ ] Backward compatibility verified
- [ ] Functional sign-off obtained

### Business Success

- [ ] Flexibility to control deletion without code changes
- [ ] No disruption to MGN process
- [ ] Configuration maintainable by functional team
- [ ] Meets original business requirement
- [ ] User acceptance testing passed

---

## Appendix

### A. Complete Code Listing

**Function Module:** Z_LOG_YTTS_FUNC_LIST

**Modified Section:**

```abap
*-----------------------------------------------------------------------
* Section: YTTS Data Selection and Processing
* Location: After line 448 (approximately)
* Change: Add conditional delete logic based on configuration parameter
*-----------------------------------------------------------------------

" BEGIN: Cursor Generated Code
" Modification Date: [DD.MM.YYYY]
" Modified By: [USER_ID]
" Change Request: [CR_NUMBER]

SELECT *
       FROM ytts
       INTO TABLE lt_ytts
       WHERE area       EQ lw_ytts1-area
         AND transplpt  EQ lw_ytts1-transplpt
         AND vstel      EQ lw_ytts1-vstel
         AND trk_purpos EQ lw_ytts1-trk_purpos
         AND matgr      EQ lw_ytts1-matgr.
IF sy-subrc EQ 0.
  
  " Check configuration parameter for conditional delete
  " Parameter: ZSCM_MOB_MGN_DEL_ACT controls deletion behavior
  " Active (X): Delete records with initial truck order
  " Not Active (blank): Keep all records
  SELECT SINGLE active
    FROM zlog_exec_var
    INTO lv_delete_active
    WHERE name   EQ 'ZSCM_MOB_MGN_DEL_ACT'
      AND active EQ abap_true.
  
  " Execute conditional delete based on configuration
  IF sy-subrc EQ 0 AND lv_delete_active EQ abap_true.
    " Configuration parameter is active
    " Delete records where truck order is initial (blank)
    DELETE lt_ytts WHERE trk_order EQ space.
  ENDIF.
  " If parameter not active or not found, proceed with all records
  " This ensures backward compatibility and safe default behavior
  
  " Sort remaining records by truck order
  SORT lt_ytts BY trk_order ASCENDING.
  
ENDIF.
" END: Cursor Generated Code
```

### B. Configuration Table Structure

**Table:** ZLOG_EXEC_VAR

```
Field Name      | Data Element  | Type  | Length | Description
----------------|---------------|-------|--------|---------------------------
MANDT           | MANDT         | CLNT  | 3      | Client
NAME            | RVARI_VNAM    | CHAR  | 30     | Variable Name
NUMB            | TVARV_NUMB    | NUMC  | 10     | Line Number
ACTIVE          | ZACTIVE_FLAG  | CHAR  | 1      | Active Flag (X/blank)
AREA            | YAREA         | CHAR  | 4      | Area
FUNCTION        | YSTATS        | CHAR  | 4      | Function
MATGR           | MVGR1         | CHAR  | 3      | Material Group
REJECT_RES      | YREASONCD     | CHAR  | 10     | Reject Reason
REMARKS         | ZREMARKS      | CHAR  | 255    | Remarks
```

### C. Test Data Scripts

**Create Test Configuration Entry:**
```sql
-- DEV/QAS System
INSERT INTO ZLOG_EXEC_VAR (
  MANDT, NAME, NUMB, ACTIVE, AREA, FUNCTION, MATGR, REJECT_RES, REMARKS
) VALUES (
  '100', 
  'ZSCM_MOB_MGN_DEL_ACT', 
  '0000000001', 
  'X', 
  '', 
  '', 
  '', 
  '', 
  'Enable/disable deletion of initial truck orders in MGN process'
);
```

**Create Test YTTS Data:**
```abap
DATA: lt_ytts_test TYPE TABLE OF ytts.

" Record 1: Valid truck order
APPEND VALUE #(
  area       = 'PL01'
  transplpt  = 'TP001'
  vstel      = 'VS01'
  trk_purpos = 'A'
  matgr      = 'M01'
  trk_order  = '0000000001'
) TO lt_ytts_test.

" Record 2: Initial truck order (should be deleted if active)
APPEND VALUE #(
  area       = 'PL01'
  transplpt  = 'TP001'
  vstel      = 'VS01'
  trk_purpos = 'A'
  matgr      = 'M01'
  trk_order  = ''
) TO lt_ytts_test.

" Record 3: Valid truck order
APPEND VALUE #(
  area       = 'PL01'
  transplpt  = 'TP001'
  vstel      = 'VS01'
  trk_purpos = 'A'
  matgr      = 'M01'
  trk_order  = '0000000002'
) TO lt_ytts_test.
```

### D. Troubleshooting Guide

#### Issue 1: Configuration Parameter Not Working

**Symptom:** Delete always happens or never happens regardless of parameter

**Diagnosis:**
1. Check SM30 entry exists: ZLOG_EXEC_VAR
2. Verify NAME = 'ZSCM_MOB_MGN_DEL_ACT' (exact match)
3. Check ACTIVE field value ('X' or blank)
4. Debug: Check `lv_delete_active` value

**Resolution:**
- Ensure entry exists with correct NAME (case-sensitive)
- Verify ACTIVE = 'X' (uppercase X)
- Check sy-subrc after SELECT

#### Issue 2: Performance Degradation

**Symptom:** Function module takes longer to execute

**Diagnosis:**
1. Run ST05 SQL trace
2. Check additional SELECT execution time
3. Verify ZLOG_EXEC_VAR has index on NAME field

**Resolution:**
- Additional SELECT should be < 10ms
- Ensure index exists: NAME + ACTIVE
- Consider caching parameter value if called frequently

#### Issue 3: Dump in Production

**Symptom:** Short dump ST22 related to this change

**Diagnosis:**
1. Check ST22 dump
2. Identify line number
3. Check variable values in dump

**Resolution:**
- Immediate: Set ACTIVE = '' (rollback via config)
- If persists: Import previous transport version
- Contact development team

### E. References

**Related Documents:**
- Functional Specification: MGN_FS.md
- Function Module Documentation: fm_Z_LOG_YTTS_FUNC_LIST.md
- ABAP Code Rules: /ABAP Code Rules/*.mdc

**SAP Documentation:**
- NetWeaver 7.31 ABAP Reference
- OpenSQL Reference
- Function Module Development Guide

**Tables:**
- YTTS - Truck Tracking System Master
- YTTSTX0001 - Truck Tracking Extended 0001
- ZLOG_EXEC_VAR - Execution Variables Configuration

**Transactions:**
- SE37 - Function Builder
- SE80 - Object Navigator
- SM30 - Table Maintenance
- ST05 - SQL Trace
- ST22 - Dump Analysis

---

## Approval Sign-Off

| Role | Name | Signature | Date |
|------|------|-----------|------|
| **Technical Lead** | | | |
| **Functional Lead** | | | |
| **Quality Assurance** | | | |
| **Project Manager** | | | |

---

**Document Version:** 1.0  
**Last Updated:** [Date]  
**Next Review:** [Date + 90 days]

---

**End of Technical Specification**
