import random
import csv
from datetime import datetime, timedelta

random.seed(42)

first_names_m = [
    "James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles",
    "Christopher", "Daniel", "Matthew", "Anthony", "Donald", "Mark", "Paul", "Steven", "Andrew", "Kenneth",
    "Joshua", "Kevin", "Brian", "George", "Timothy", "Ronald", "Edward", "Jason", "Jeffrey", "Ryan",
    "Jacob", "Gary", "Nicholas", "Eric", "Jonathan", "Stephen", "Larry", "Justin", "Scott", "Brandon",
    "Benjamin", "Samuel", "Gregory", "Alexander", "Patrick", "Frank", "Raymond", "Jack", "Dennis", "Jerry",
    "Tyler", "Aaron", "Jose", "Adam", "Nathan", "Henry", "Douglas", "Zachary", "Peter", "Kyle",
    "Noah", "Ethan", "Jeremy", "Christian", "Walter", "Keith", "Austin", "Roger", "Terry", "Sean",
    "Gerald", "Carl", "Dylan", "Harold", "Jordan", "Jesse", "Bryan", "Lawrence", "Arthur", "Gabriel",
    "Bruce", "Logan", "Billy", "Albert", "Willie", "Alan", "Juan", "Wayne", "Elijah", "Randy",
    "Roy", "Vincent", "Ralph", "Eugene", "Russell", "Bobby", "Mason", "Philip", "Louis", "Harry"
]

first_names_f = [
    "Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Karen",
    "Lisa", "Nancy", "Betty", "Margaret", "Sandra", "Ashley", "Kimberly", "Emily", "Donna", "Michelle",
    "Carol", "Amanda", "Dorothy", "Melissa", "Deborah", "Stephanie", "Rebecca", "Sharon", "Laura", "Cynthia",
    "Kathleen", "Amy", "Angela", "Shirley", "Anna", "Brenda", "Pamela", "Emma", "Nicole", "Helen",
    "Samantha", "Katherine", "Christine", "Debra", "Rachel", "Carolyn", "Janet", "Maria", "Heather", "Diane",
    "Julie", "Joyce", "Victoria", "Kelly", "Christina", "Lauren", "Joan", "Evelyn", "Olivia", "Judith",
    "Megan", "Cheryl", "Martha", "Andrea", "Frances", "Hannah", "Jacqueline", "Ann", "Gloria", "Jean",
    "Kathryn", "Alice", "Teresa", "Sara", "Janice", "Doris", "Madison", "Julia", "Grace", "Judy",
    "Abigail", "Marie", "Denise", "Beverly", "Amber", "Theresa", "Marilyn", "Diana", "Danielle", "Jane",
    "Brittany", "Natalie", "Sophia", "Rose", "Isabella", "Alexis", "Kayla", "Charlotte", "Valerie", "Monica"
]

last_names = [
    "Smith", "Johnson", "Williams", "Brown", "Jones", "Garcia", "Miller", "Davis", "Rodriguez", "Martinez",
    "Hernandez", "Lopez", "Gonzalez", "Wilson", "Anderson", "Thomas", "Taylor", "Moore", "Jackson", "Martin",
    "Lee", "Perez", "Thompson", "White", "Harris", "Sanchez", "Clark", "Ramirez", "Lewis", "Robinson",
    "Walker", "Young", "Allen", "King", "Wright", "Scott", "Torres", "Nguyen", "Hill", "Flores",
    "Green", "Adams", "Nelson", "Baker", "Hall", "Rivera", "Campbell", "Mitchell", "Carter", "Roberts",
    "Gomez", "Phillips", "Evans", "Turner", "Diaz", "Parker", "Cruz", "Edwards", "Collins", "Reyes",
    "Stewart", "Morris", "Morales", "Murphy", "Cook", "Rogers", "Gutierrez", "Ortiz", "Morgan", "Cooper",
    "Peterson", "Bailey", "Reed", "Kelly", "Howard", "Ramos", "Kim", "Cox", "Ward", "Richardson",
    "Watson", "Brooks", "Chavez", "Wood", "James", "Bennett", "Gray", "Mendoza", "Ruiz", "Hughes",
    "Price", "Alvarez", "Castillo", "Sanders", "Patel", "Myers", "Long", "Ross", "Foster", "Jimenez"
]

departments_diseases = {
    "Cardiology": [
        ("Coronary Artery Disease", ["Angioplasty", "Medication Therapy", "Conservative Monitoring"], ["Atorvastatin", "Amlodipine", "Lisinopril", "Aspirin"], (45, 88), 12000, 32000),
        ("Hypertension", ["Medication Therapy", "Dietary Counseling", "Conservative Monitoring"], ["Lisinopril", "Amlodipine", "Losartan"], (35, 85), 3500, 9500),
        ("Heart Failure", ["Medication Therapy", "Mechanical Ventilation", "Conservative Monitoring"], ["Furosemide", "Carvedilol", "Lisinopril"], (50, 90), 14000, 42000),
    ],
    "Neurology": [
        ("Stroke", ["Thrombolytic Therapy", "Physiotherapy", "Medication Therapy"], ["Heparin", "Aspirin", "Alteplase"], (48, 89), 16000, 45000),
        ("Migraine", ["Medication Therapy", "Pain Management", "Conservative Monitoring"], ["Sumatriptan", "Propranolol", "Topiramate"], (18, 65), 2200, 6800),
        ("Parkinson's Disease", ["Medication Therapy", "Physiotherapy"], ["Levodopa", "Pramipexole"], (55, 88), 8500, 24000)
    ],
    "Orthopedics": [
        ("Osteoarthritis", ["Knee Replacement", "Physiotherapy", "Joint Injections"], ["Celecoxib", "Tramadol", "Acetaminophen"], (50, 85), 18000, 38000),
        ("Bone Fracture", ["Orthopedic Surgery", "Cast Immobilization", "Physiotherapy"], ["Codeine", "Acetaminophen", "Cefazolin"], (12, 80), 9000, 26000),
        ("Spinal Stenosis", ["Spinal Decompression", "Physiotherapy", "Epidural Steroid"], ["Gabapentin", "Meloxicam"], (45, 82), 15000, 35000)
    ],
    "Pulmonology": [
        ("Pneumonia", ["Antibiotic Regimen", "Oxygen Therapy", "Mechanical Ventilation"], ["Ceftriaxone", "Azithromycin", "Levofloxacin"], (15, 89), 6500, 22000),
        ("Asthma", ["Inhalation Therapy", "Oxygen Therapy", "Steroid Regimen"], ["Albuterol", "Fluticasone", "Prednisone"], (5, 75), 3200, 11000),
        ("COPD", ["Bronchodilator Therapy", "Oxygen Therapy", "Pulmonary Rehab"], ["Tiotropium", "Salmeterol", "Prednisone"], (52, 88), 8000, 28000)
    ],
    "Gastroenterology": [
        ("Gallstones", ["Laparoscopic Cholecystectomy", "Medication Therapy"], ["Ursodiol", "Ciprofloxacin", "Ketorolac"], (25, 78), 11000, 27000),
        ("Gastritis", ["Acid Suppression Therapy", "Dietary Management"], ["Omeprazole", "Pantoprazole", "Sucralfate"], (20, 75), 2800, 7500),
        ("Liver Cirrhosis", ["Paracentesis", "Medication Therapy", "Nutritional Support"], ["Spironolactone", "Lactulose", "Rifaximin"], (40, 80), 14000, 39000)
    ],
    "Nephrology": [
        ("Chronic Kidney Disease", ["Hemodialysis", "Medication Therapy", "Fluid Restriction"], ["Erythropoietin", "Calcium Acetate", "Sevelamer"], (38, 86), 15000, 48000),
        ("Kidney Stones", ["Lithotripsy", "Hydration Therapy", "Pain Management"], ["Tamsulosin", "Ketorolac", "Acetaminophen"], (22, 70), 5500, 16000)
    ],
    "General Surgery": [
        ("Appendicitis", ["Laparoscopic Appendectomy", "Antibiotic Regimen"], ["Cefoxitin", "Metronidazole", "Morphine"], (12, 65), 10500, 25000),
        ("Hernia", ["Hernioplasty", "Laparoscopic Mesh Repair"], ["Cefazolin", "Ibuprofen"], (25, 75), 8500, 19500)
    ],
    "Emergency": [
        ("Severe Trauma", ["Emergency Trauma Resuscitation", "Surgical Intervention", "ICU Care"], ["Fentanyl", "Tranexamic Acid", "Cefazolin"], (16, 75), 20000, 52000),
        ("Acute Intoxication", ["Gastric Lavage", "Supportive Care", "IV Hydration"], ["Naloxone", "Thiamine", "Dextrose"], (18, 65), 4000, 12000)
    ],
    "Pediatrics": [
        ("Pediatric Bronchitis", ["Nebulizer Therapy", "Hydration Therapy", "Antibiotic Regimen"], ["Amoxicillin", "Albuterol", "Acetaminophen"], (1, 14), 2200, 7500),
        ("Viral Gastroenteritis", ["Oral Rehydration", "Antiemetic Therapy"], ["Ondansetron", "Electrolyte Solution"], (1, 15), 1800, 5500)
    ]
}

doctors_by_dept = {
    "Cardiology": ["Dr. Robert Chen", "Dr. Sarah Jenkins"],
    "Neurology": ["Dr. Emily Davis", "Dr. David Martinez"],
    "Orthopedics": ["Dr. James Wilson", "Dr. Mark Robinson"],
    "Pulmonology": ["Dr. Lisa Taylor", "Dr. Priya Sharma"],
    "Gastroenterology": ["Dr. Michael Patel", "Dr. Angela White"],
    "Nephrology": ["Dr. Robert Chen", "Dr. David Martinez"],
    "General Surgery": ["Dr. James Wilson", "Dr. Angela White"],
    "Emergency": ["Dr. Mark Robinson", "Dr. Michael Patel"],
    "Pediatrics": ["Dr. Lisa Taylor", "Dr. Sarah Jenkins"]
}

blood_groups = ["A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-"]
blood_group_weights = [0.31, 0.06, 0.15, 0.02, 0.05, 0.01, 0.36, 0.04]

admission_types = ["Emergency", "Elective", "Urgent", "Trauma", "Routine"]
admission_type_weights = [0.38, 0.28, 0.18, 0.06, 0.10]

insurances = ["Medicare", "Blue Cross Blue Shield", "UnitedHealthcare", "Aetna", "Cigna", "Humana", "Medicaid", "Self-Pay"]
insurance_weights = [0.28, 0.22, 0.18, 0.12, 0.08, 0.05, 0.04, 0.03]

start_date = datetime(2024, 1, 1)
end_date = datetime(2025, 12, 20)
delta_days = (end_date - start_date).days

total_records = 1500
records = []

for i in range(1, total_records + 1):
    pat_id = f"PAT-{1000 + i}"
    
    # Gender
    gender_r = random.random()
    if gender_r < 0.49:
        gender = "Male"
        name = f"{random.choice(first_names_m)} {random.choice(last_names)}"
    elif gender_r < 0.98:
        gender = "Female"
        name = f"{random.choice(first_names_f)} {random.choice(last_names)}"
    else:
        gender = "Other"
        name = f"{random.choice(first_names_m if random.random() < 0.5 else first_names_f)} {random.choice(last_names)}"
        
    # Department
    dept = random.choice(list(departments_diseases.keys()))
    disease_info = random.choice(departments_diseases[dept])
    disease, treatments, medications, (age_min, age_max), cost_min, cost_max = disease_info
    
    age = random.randint(age_min, age_max)
    doctor = random.choice(doctors_by_dept[dept])
    treatment = random.choice(treatments)
    medication = random.choice(medications)
    
    blood_group = random.choices(blood_groups, weights=blood_group_weights, k=1)[0]
    
    if dept == "Emergency":
        adm_type = random.choices(["Emergency", "Trauma", "Urgent"], weights=[0.6, 0.25, 0.15], k=1)[0]
    elif dept == "Pediatrics":
        adm_type = random.choices(["Urgent", "Emergency", "Routine"], weights=[0.5, 0.3, 0.2], k=1)[0]
    elif "Surgery" in treatment or "Replacement" in treatment or "Appendectomy" in treatment:
        adm_type = random.choices(["Elective", "Emergency", "Urgent"], weights=[0.5, 0.3, 0.2], k=1)[0]
    else:
        adm_type = random.choices(admission_types, weights=admission_type_weights, k=1)[0]
        
    insurance = random.choices(insurances, weights=insurance_weights, k=1)[0]
    if age >= 65 and random.random() < 0.75:
        insurance = "Medicare"
        
    # Dates
    adm_days_offset = random.randint(0, delta_days)
    adm_dt = start_date + timedelta(days=adm_days_offset)
    adm_date_str = adm_dt.strftime("%Y-%m-%d")
    
    # Length of stay
    if "Surgery" in treatment or "Ventilation" in treatment or "Hemodialysis" in treatment:
        los = random.randint(4, 21)
    elif adm_type in ["Emergency", "Trauma"]:
        los = random.randint(2, 16)
    else:
        los = random.randint(1, 8)
        
    disch_dt = adm_dt + timedelta(days=los)
    
    # Realistic Patient Status based on age, severity, treatment
    is_active = (disch_dt > end_date) or (random.random() < 0.05 and adm_dt > datetime(2025, 11, 1))
    
    if is_active:
        disch_date_str = ""
        los_actual = (end_date - adm_dt).days if (end_date - adm_dt).days > 0 else 1
        if age > 75 and random.random() < 0.25:
            status = "Critical"
        else:
            status = "Under Treatment"
    else:
        disch_date_str = disch_dt.strftime("%Y-%m-%d")
        los_actual = los
        
        # Determine outcome
        outcome_rand = random.random()
        if age > 75 and disease in ["Heart Failure", "Stroke", "Severe Trauma"] and outcome_rand < 0.06:
            status = "Deceased"
        elif outcome_rand < 0.03:
            status = "Transferred"
        elif outcome_rand < 0.40:
            status = "Recovered"
        else:
            status = "Discharged"
            
    # Billing amount
    base_cost = random.randint(cost_min, cost_max)
    los_multiplier = 1.0 + (los_actual * 0.08)
    severity_mult = 1.35 if status in ["Critical", "Deceased"] else (1.15 if adm_type in ["Emergency", "Trauma"] else 1.0)
    billing_amount = round(base_cost * los_multiplier * severity_mult * random.uniform(0.92, 1.08), 2)
    
    records.append({
        "Patient_ID": pat_id,
        "Name": name,
        "Age": age,
        "Gender": gender,
        "Blood_Group": blood_group,
        "Disease": disease,
        "Department": dept,
        "Doctor": doctor,
        "Admission_Date": adm_date_str,
        "Discharge_Date": disch_date_str,
        "Admission_Type": adm_type,
        "Treatment": treatment,
        "Medication": medication,
        "Insurance": insurance,
        "Billing_Amount": billing_amount,
        "Patient_Status": status,
        "Length_of_Stay": los_actual
    })

# Add 2 duplicate records to demonstrate real data quality / cleaning functionality
records.append(records[10].copy())
records.append(records[42].copy())

fieldnames = [
    "Patient_ID", "Name", "Age", "Gender", "Blood_Group", "Disease", "Department",
    "Doctor", "Admission_Date", "Discharge_Date", "Admission_Type", "Treatment",
    "Medication", "Insurance", "Billing_Amount", "Patient_Status", "Length_of_Stay"
]

with open("data/hospital_patients.csv", "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writeheader()
    for row in records:
        writer.writerow(row)

print(f"Generated {len(records)} records in data/hospital_patients.csv successfully.")
