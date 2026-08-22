# استدعاء المكتبات (Libraries)
# تمنحك أدوات التعامل مع الجداول وتعديل البيانات (مثل select, mutate, filter).
library(dplyr) 
library(tidyverse)
# مكتبة متخصصة للتعامل مع التواريخ واستخراج الأيام والشهور والسنوات بسهولة.
library(lubridate)

#تقرأ ملف الـ CSV من الجهاز وتحوله إلى جدول بيانات (Data Frame) داخل R.
data <- read.csv('COVID-19_Case_Surveillance_Public_Use_Data.csv', nrows = 500000)
# تعرض أول 6 أسطر من الجدول للتأكد من المسميات وأنواع العمود.
head(data)

# معالجة البيانات وبناء المتغيرات التحليلية , تصفية الأعمدة واختيار المهم منها (Column Selection)
cleaned_data <- data %>%
  # اختيار الأعمدة الأساسية وتصفية القيم الخاطئة
  # تُستخدم لاختيار الأعمدة التي نحتاجها فقط واستبعاد الأعمدة الزائدة لتقليل حجم الملف وسرعة الأداء.
  select(
    cdc_report_dt, pos_spec_dt, onset_dt, current_status, sex, age_group, Race.and.ethnicity..combined., hosp_yn, icu_yn, death_yn, medcond_yn
  )  %>% 
  # الدالة الأساسية في R لإضافة أعمدة جديدة أو تعديل أعمدة قائمة.
  mutate(
    # تحويل التواريخ بشكل مرن يتقبل YYYY/MM/DD و YYYY-MM-DD
    cdc_report_dt = parse_date_time(cdc_report_dt, orders = c("ymd", "Ymd", "Y/m/d")),
    pos_spec_dt = parse_date_time(pos_spec_dt, orders = c("ymd", "Ymd", "Y/m/d")),
    
    # تحويل التواريخ
    cdc_report_dt = as.Date(cdc_report_dt),
    pos_spec_dt = as.Date (pos_spec_dt),
    # استخراج السنة والشهر
    year = year(cdc_report_dt),
    month = month(cdc_report_dt, label = TRUE, abbr = TRUE),
    # حساب زمن تأخير الإبلاغ باليوم لتحديد SLA | طرح تاريخين لمعرفة عدد أيام التأخير.
    # as.numeric(): تحول النتيجة إلى رقم مجرد.
    reporting_delay_days = ifelse(!is.na(cdc_report_dt)& !is.na(pos_spec_dt), as.numeric(cdc_report_dt - pos_spec_dt), NA_real_),
    # دالة شرطية؛ إذا كان التأخير أقل من أو يساوي 3 أيام يُصنف كـ "Within SLA"، وإلا يُصنف كـ "Out of SLA"
    within_sla = case_when(
      is.na(reporting_delay_days) ~ "Unknown",
            reporting_delay_days <= 3 ~ "within SLA",
            TRUE        ~ "Out of SLA"
    ),
    # تصنيف درجة الخطورة (Severity)
    # case_when(): تُستخدم عند وجود شروط متعددة (أشبه بـ IF المتعددة):
    # | تعني OR
    severity = case_when(
      death_yn == "Yes" | icu_yn == "Yes" ~ "Urgent",
      hosp_yn == "Yes"                    ~ "Major",
      hosp_yn == "No"                     ~ "Normal",
      TRUE                                ~ "Low/Unknown"
    ) ,
    # %in%: تتحقق ما إذا كانت القيمة تنتمي لمجموعة معينة (Male أو Female). إذا كانت القيمة غير ذلك (مثل Missing أو Unknown) تستبدلها بـ "Unknown"
    # تنظيف القيم المفقودة في المتغيرات النصية
    sex = if_else(sex %in% c("Male", "Female"), sex, "Unknown"),
    # is.na(): تفحص ما إذا كانت الخلية فارغة تماماً (NA).
    age_group = if_else(is.na(age_group) | age_group == "Missing", "Unknown", age_group),
    ethnicity = if_else(is.na(Race.and.ethnicity..combined.), "Unknown", Race.and.ethnicity..combined.)
  ) %>%
  # تصفية السجلات التي لا تحتوي على تاريخ إبلاغ صحيح
  # filter(): تُبقي فقط على الصفوف التي تحقق الشرط.
  # !is.na(): علامة التعجب ! تعني "ليس"؛ أي استبعد الصفوف التي يكون فيها تاريخ الإبلاغ فارغاً.
  filter(!is.na(cdc_report_dt))


  # حفظ البيانات المعالجة كملف CSV جديد

write_csv(cleaned_data, "covid_surveillance_cleaned_for_powerbi.csv")
# cat(): تطبع نصاً تأكيدياً في الشاشة (Console) عند اكتمال العملية بنجاح.
cat("✅ تم تنظيف البيانات بنجاح وحفظها للعمل عليها في Power BI!")
