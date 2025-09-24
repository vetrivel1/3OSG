# MBCNTR2503 Legacy Pipeline Flow Analysis

Based on `mbcntr2503.script`, here is the documented step-by-step flow:

## 📋 **Pipeline Parameters**
```bash
job=$1                    # Job number (e.g., 69172)
source=$2                 # Source file (e.g., "test")
Client=2503              # Client code
Work2Len=4300            # Work2 record length
ProjectBase=/users/programs/container
Project=mblps            # Project name
ContainerKey=1941        # Container key
OptionLen=2000           # Option record length
ClientDept=250301        # Client department
ServiceType=320          # Service type
PieceAttribute=ncp1stclass1ozimbnew
```

## 🔄 **Step-by-Step Pipeline Flow**

### **Step 1: Supplemental Table Setup**
```bash
# Legacy command:
cp /users/programs/2503supptable.txt /users/public/$job.se1

# Files created:
- 69172.se1 (supplemental table copy)
```

### **Step 2: Container Processing (ncpcntr5v2.script)**
```bash
# Legacy command:
ncpcntr5v2.script j-$job $InPath c-$Client 2-$Work2Len r-$Project e-$ProjectBase

# Parameters:
- j-69172 (job number)
- /users/public/69172.dat (input path)
- c-2503 (client)
- 2-4300 (work2 length)
- r-mblps (project)
- e-/users/programs/container (project base)

# Files created:
- 69172e.txt (container processed output)
```

### **Step 3: Convert to Option Records (setmb2000.script)**
```bash
# Legacy command:
/users/scripts/setmb2000.script 0503 $job $job.dat

# Parameters:
- 0503 (option code)
- 69172 (job number)
- 69172.dat (input file)

# Files modified:
- Converts records to 2000-byte option format
```

### **Step 4: E-bill Split (cnpsplit4.out)**
```bash
# Legacy command:
/users/programs/cnpsplit4.out 2000 1318 1 E 0 0 /users/public/69172e.txt ASCII /users/public/69172p.asc

# Parameters:
- 2000 (record length)
- 1318 (selection offset)
- 1 (selection length)
- E (selection value)
- 0 (max reads - unlimited)
- 0 (max matches - unlimited)
- /users/public/69172e.txt (input file)
- ASCII (format)
- /users/public/69172p.asc (output file)

# File operations:
mv /users/public/69172p.asc /users/public/69172p.asc.org
mv /users/public/69172p.asc.match /users/public/69172e.asc
mv /users/public/69172p.asc.unmatch /users/public/69172p.asc

# Files created:
- 69172p.asc.org (original split output)
- 69172e.asc (matched records)
- 69172p.asc (unmatched records)
```

### **Step 5: Process Split Files (ncpcntr0v2.script)**
```bash
# Legacy command:
find /users/public/69172*.asc -prune -size +0c -exec ncpcntr0v2.script 0-YES {} h-2000 r-mblps e-/users/programs/container j-69172 c-2503 u-ncp1stclass1ozimbnew d-250301 t-320 g-100000 k-1941 2-4300 \;

# Parameters for each .asc file:
- 0-YES (option flag)
- h-2000 (option length)
- r-mblps (project)
- e-/users/programs/container (project base)
- j-69172 (job number)
- c-2503 (client)
- u-ncp1stclass1ozimbnew (piece attribute)
- d-250301 (client department)
- t-320 (service type)
- g-100000 (group size)
- k-1941 (container key)
- 2-4300 (work2 length)

# Files created:
- 69172p.cntr.grp (processed container group)
- 69172e.cntr.grp (processed e-bill group)
```

### **Step 6: Add Product Codes (tagit10.script)**
```bash
# Legacy commands:
find *69172p.cntr.grp -exec /users/moonpie/tagit10.script {} 1000 0170 N \;
/users/moonpie/tagit10.script 69172e.cntr.grp 1041 0170 N

# Parameters:
- 1000/1041 (product codes)
- 0170 (position)
- N (flag)

# Files modified:
- Adds product codes to .cntr.grp files
```

### **Step 7: First Pass GMC (General Mail Classification)**
```bash
# Legacy command:
find *69172*.cntr.grp -exec ssh 207.98.205.66 'cmd /c d:\scripts\mbcntrfirstpassgmc.bat' {} gjs_002503_0001_lpsbill.wfd \;

# Files created:
- 69172*.cntr.grp.txt (GMC output files)

# Auto count processing:
find /users/public/*69172*.cntr.grp.txt -exec /users/programs/cnpautocount.out 266 33 3 SEPARATE {} \;
cat /users/public/*69172*.cntr.grp.txt.33.3.*.auto > /users/public/69172count.auto

# Files created:
- 69172count.auto (consolidated auto counts)
```

### **Step 8: Generate Samples and Totals (ncpcntrsample0.out)**
```bash
# Legacy commands:
if [ -s a69172p.cntr.grp ]; then 
    /users/programs/ncpcntrsample0.out /users/public/a69172p.cntr.grp P0001 97 1
    mv /users/public/a69172p.cntr.grp.total /users/public/69172p.cntr.grp.total
    mv /users/public/a69172p.cntr.grp.sample /users/public/69172p.cntr.grp.sample
else 
    /users/programs/ncpcntrsample0.out /users/public/69172p.cntr.grp P0001 97 1
fi
                  
/users/programs/ncpcntrsample0.out /users/public/69172e.cntr.grp P0001 97 1

# Parameters:
- P0001 (sample type)
- 97 (sample position)
- 1 (sample flag)

# Files created:
- 69172p.cntr.grp.total
- 69172p.cntr.grp.sample
- 69172e.cntr.grp.total
- 69172e.cntr.grp.sample
```

### **Step 9: Auto Label Generation (cnpautolabel2.out)**
```bash
# Legacy command:
/users/programs/cnpautolabel2.out MBILLCNTR 2503 test 69172

# Parameters:
- MBILLCNTR (label type)
- 2503 (client)
- test (source)
- 69172 (job number)

# Files created:
- Auto-generated labels
```

### **Step 10: Create Consolidated Outputs**
```bash
# Legacy command:
cat /users/public/*69172*.cntr.grp > /users/public/69172.all.grp

# Files created:
- 69172.all.grp (consolidated group file)
```

### **Step 11: Mail Tracking Container**
```bash
# Legacy commands:
cntrimbremit.script 69172 /users/public/69172p.cntr.grp 1 000546 050 1000 2503
cnpimbremit1.script 69172 2503 /users/public

# Parameters:
- 1 (sequence)
- 000546 (tracking code)
- 050 (mail class)
- 1000 (container size)
- 2503 (client)

# Files created:
- Mail tracking and remittance files
```

## 📊 **Expected Output Files**
Based on the legacy script, these files should be created:

### Primary Output Files:
- `69172.se1` - Supplemental table
- `69172e.txt` - Container processed output
- `69172p.asc.org` - Original split output  
- `69172e.asc` - E-bill matched records
- `69172p.asc` - Unmatched records
- `69172p.cntr.grp` - Processed container group
- `69172e.cntr.grp` - Processed e-bill group
- `69172count.auto` - Auto count totals
- `69172p.cntr.grp.total` - Sample totals
- `69172p.cntr.grp.sample` - Sample records
- `69172e.cntr.grp.total` - E-bill totals
- `69172e.cntr.grp.sample` - E-bill samples
- `69172.all.grp` - Consolidated group file

### Work2 Files (4300-byte records):
- `69172.4300` - Binary work2 file
- `69172.4300.txt` - Text work2 file
- `69172.4300.txt.new` - Formatted work2 file
- `69172.4300.txt.length` - Record count
- `69172.4300.txt.suspect` - Suspect records

## 🎯 **Testing Strategy**
We should compare our pipeline output against legacy at each step:

1. **Step 1**: Compare `69172.se1` supplemental table
2. **Step 2**: Compare `69172e.txt` container output
3. **Step 4**: Compare split files (`69172p.asc.org`, `69172e.asc`, `69172p.asc`)
4. **Step 8**: Compare work2 files (`69172.4300*` series)
5. **Step 10**: Compare `69172.all.grp` consolidated output

This will help us identify exactly where our pipeline differs from the legacy system.