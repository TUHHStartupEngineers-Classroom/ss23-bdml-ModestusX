employee_attrition_tbl <- read_csv("AutoML 1/datasets-1067-1925-WA_Fn-UseC_-HR-Employee-Attrition.csv")
# Business & Data Understanding: Department and Job Role

# Data subset
dept_job_role_tbl <- employee_attrition_tbl %>%
  select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

dept_job_role_tbl %>%
  
  group_by(Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(pct = n / sum(n))

# Attrition by department
dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2: Caution: It's easy to inadvertently miss grouping when creating counts & percents within groups
  group_by(Department) %>%
  mutate(pct = n / sum(n))

# Attrition by job role
dept_job_role_tbl %>%
  
  # Block 1
  group_by(Department, JobRole, Attrition) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  
  # Block 2
  group_by(Department, JobRole) %>%
  mutate(pct = n / sum(n)) %>%
  ungroup() %>%
  
  # Block 3
  filter(Attrition %in% "Yes")

