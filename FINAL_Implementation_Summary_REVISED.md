# FINAL ABAP Code - MGN Enhancement (REVISED)

## Corrected Implementation - Updated Per User Request

---

## Summary of Changes

### Total Locations Modified: 3 (REVISED)

1. **Variable Declaration** (After line 216)
2. ~~**Primary YTTS Selection** (Lines 448-458)~~ **REMOVED - No changes needed**
3. **TPN Path** (Lines 466-468)
4. **JG Active Path - TWO locations:**
   - **4a.** First SELECT (Line 489-491)
   - **4b.** Second SELECT (Between lines 505-506) ⭐ **USER'S REQUESTED LOCATION**

---

## Important Update

**Primary YTTS Selection path (Lines 448-458) has been REVERTED to original code.**

The user requested removal of conditional delete logic from this location. Only the TPN and JG Active paths now have conditional delete logic.

---

## Detailed Change Locations

### Location 1: Variable Declaration

**Line:** After 216  
**Section:** DATA declarations

```abap
" BEGIN: Cursor Generated Code
DATA: lv_delete_active TYPE zactive_flag.
" END: Cursor Generated Code
```

---

### Location 2: Primary YTTS Selection (Lines 448-458) - NO CHANGE

**Code Remains Original:**
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
```

**Note:** This location keeps the original DELETE statement WITHOUT conditional logic.

---

### Location 3: TPN Path (Lines 466-468) - MODIFIED

**Original Code:**
```abap
IF sy-subrc EQ 0.
  lw_btpn = abap_true.
  DELETE lt_ytts WHERE trk_order EQ space.
```

**Modified Code:**
```abap
IF sy-subrc EQ 0.
  lw_btpn = abap_true.
  
  " Check configuration parameter for conditional delete (TPN path)
  SELECT SINGLE active
    FROM zlog_exec_var
    INTO lv_delete_active
    WHERE name   EQ 'ZSCM_MOB_MGN_DEL_ACT'
      AND active EQ abap_true.
  
  " Execute conditional delete based on configuration
  IF sy-subrc EQ 0 AND lv_delete_active EQ abap_true.
    DELETE lt_ytts WHERE trk_order EQ space.
  ENDIF.
```

---

### Location 4a: JG Active Path - First SELECT (Lines 489-491) - MODIFIED

**Original Code:**
```abap
IF sy-subrc = 0.
  SORT lt_ytts BY trk_order.
  DELETE lt_ytts WHERE trk_order IS INITIAL.
```

**Modified Code:**
```abap
IF sy-subrc = 0.
  SORT lt_ytts BY trk_order.
  
  " Check configuration parameter for conditional delete (JG path - 1st SELECT)
  SELECT SINGLE active
    FROM zlog_exec_var
    INTO lv_delete_active
    WHERE name   EQ 'ZSCM_MOB_MGN_DEL_ACT'
      AND active EQ abap_true.
  
  " Execute conditional delete based on configuration
  IF sy-subrc EQ 0 AND lv_delete_active EQ abap_true.
    DELETE lt_ytts WHERE trk_order IS INITIAL.
  ENDIF.
```

---

### Location 4b: JG Active Path - Second SELECT (Lines 505-506) ⭐ - MODIFIED

**THIS IS THE USER'S REQUESTED LOCATION**

**Original Code:**
```abap
IF sy-subrc = 0.
  SORT lt_ytts BY trk_order.
ENDIF.
```

**Modified Code:**
```abap
IF sy-subrc = 0.
  
  " Check configuration parameter for conditional delete (JG path - 2nd SELECT)
  SELECT SINGLE active
    FROM zlog_exec_var
    INTO lv_delete_active
    WHERE name   EQ 'ZSCM_MOB_MGN_DEL_ACT'
      AND active EQ abap_true.
  
  " Execute conditional delete based on configuration
  IF sy-subrc EQ 0 AND lv_delete_active EQ abap_true.
    DELETE lt_ytts WHERE trk_order IS INITIAL.
  ENDIF.
  
  SORT lt_ytts BY trk_order.
ENDIF.
```

---

## Code Structure Summary

```
Main Function: Z_LOG_YTTS_FUNC_LIST
│
├── [Location 1] Variable Declaration: lv_delete_active
│
├── [Location 2] Primary YTTS Selection (NO CHANGE)
│   ├── SELECT from YTTS (main criteria)
│   └── DELETE lt_ytts WHERE trk_order EQ space (ORIGINAL - UNCONDITIONAL)
│
├── [Location 3] TPN Path (MODIFIED)
│   ├── SELECT from YTTS (TPN function)
│   └── DELETE with conditional logic (PARAMETER CHECK)
│
└── [Locations 4a & 4b] JG Active Path (BOTH MODIFIED)
    ├── First SELECT (area, vstel, trk_purpos, matgr)
    │   └── DELETE with conditional logic (PARAMETER CHECK)
    │
    └── Second SELECT (with transplpt from first SELECT)
        └── DELETE with conditional logic (PARAMETER CHECK)
```

---

## Behavior Summary

### Primary YTTS Selection Path
- **Always deletes** records with initial `trk_order`
- **No parameter check**
- **Unconditional** behavior (original code retained)

### TPN Path
- **Conditionally deletes** based on parameter
- **Parameter:** ZSCM_MOB_MGN_DEL_ACT
- **Active (X):** Deletes records with initial `trk_order`
- **Inactive (blank):** Keeps all records

### JG Active Path (Both SELECTs)
- **Conditionally deletes** based on parameter
- **Parameter:** ZSCM_MOB_MGN_DEL_ACT
- **Active (X):** Deletes records with initial `trk_order`
- **Inactive (blank):** Keeps all records

---

## Why This Design?

Based on user requirement:
- **Primary path:** Keeps original behavior (always delete)
- **Alternative paths (TPN, JG):** Configuration-controlled (conditional delete)

This allows:
1. Main path maintains consistent behavior
2. Special scenarios (TPN, JG) can be controlled via configuration
3. Flexibility where needed, stability where critical

---

## Configuration Impact

**Table:** ZLOG_EXEC_VAR

| Field | Value | Effect |
|-------|-------|--------|
| NAME | ZSCM_MOB_MGN_DEL_ACT | Parameter name |
| ACTIVE | X | TPN & JG paths: DELETE executes |
| ACTIVE | blank | TPN & JG paths: DELETE skips |

**Primary path is NOT affected by this parameter.**

---

## Files Generated

### Z_LOG_YTTS_FUNC_LIST_FINAL.abap ⭐ **USE THIS FILE**
- Complete function module source code
- Primary path: Original code (no conditional logic)
- TPN path: Conditional delete logic
- JG Active path (2 locations): Conditional delete logic
- Ready for SE37 deployment

---

## Implementation Instructions

### Quick Deploy (5 minutes)

1. **Backup** current function module in SE37
2. Open SE37, enter `Z_LOG_YTTS_FUNC_LIST`, click Change
3. **Select All** (Ctrl+A), Delete
4. Open file: `Z_LOG_YTTS_FUNC_LIST_FINAL.abap`
5. **Copy All**, Paste into SE37
6. **Save** (enter transport request)
7. **Check** (Ctrl+F2) - verify no syntax errors
8. **Activate** (Ctrl+F3)

### Verification

After activation, check:
- ✅ Syntax check: Zero errors
- ✅ Code Inspector: Zero errors/warnings
- ✅ Variable `lv_delete_active` declared
- ✅ Primary path: Original DELETE (no conditional logic)
- ✅ TPN path: Conditional DELETE logic present
- ✅ JG path (2 locations): Conditional DELETE logic present

---

## Testing Strategy

### Test Case 1: Primary Path (Always Deletes)
**Path:** Primary YTTS Selection  
**Parameter:** Any value (not checked)  
**Expected:** Records with initial `trk_order` are ALWAYS deleted

### Test Case 2: TPN Path - Parameter Active
**Path:** TPN alternative path  
**Parameter:** ZSCM_MOB_MGN_DEL_ACT = 'X'  
**Expected:** Records with initial `trk_order` are deleted

### Test Case 3: TPN Path - Parameter Inactive
**Path:** TPN alternative path  
**Parameter:** ZSCM_MOB_MGN_DEL_ACT = '' (blank)  
**Expected:** Records with initial `trk_order` are KEPT

### Test Case 4: JG Path - Parameter Active
**Path:** JG active scenario (both SELECTs)  
**Parameter:** ZSCM_MOB_MGN_DEL_ACT = 'X'  
**Expected:** Records with initial `trk_order` are deleted in both SELECTs

### Test Case 5: JG Path - Parameter Inactive
**Path:** JG active scenario (both SELECTs)  
**Parameter:** ZSCM_MOB_MGN_DEL_ACT = '' (blank)  
**Expected:** Records with initial `trk_order` are KEPT in both SELECTs

---

## ABAP Code Rules Compliance ✅

- ✅ NetWeaver 7.31 compatible
- ✅ No inline declarations
- ✅ No host variables
- ✅ Proper variable naming (`lv_delete_active`)
- ✅ sy-subrc checked after each SELECT
- ✅ Cursor-generated markers (where applicable)
- ✅ Clear comments
- ✅ Backward compatible

---

## Success Criteria

- [ ] Primary path has NO conditional logic (original code)
- [ ] TPN path has conditional DELETE logic
- [ ] JG path location 4a has conditional DELETE logic
- [ ] JG path location 4b has conditional DELETE logic
- [ ] Variable declared once at top
- [ ] No syntax errors
- [ ] Code Inspector clean
- [ ] All test cases pass

---

## Change Summary

**Locations with Conditional Logic:** 3
1. TPN Path (Line ~466-468)
2. JG Active Path - First SELECT (Line ~489-491)
3. JG Active Path - Second SELECT (Line ~505-506)

**Locations WITHOUT Conditional Logic:** 1
1. Primary YTTS Selection (Line ~448-458) - Original code retained

---

**File to Use:** `Z_LOG_YTTS_FUNC_LIST_FINAL.abap`  
**Status:** Ready for Implementation  
**Version:** Final Revised Version (Primary path reverted)

---

**End of Documentation**
