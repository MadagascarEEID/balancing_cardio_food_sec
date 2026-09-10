library(patchwork)

model_data_acti_plots <- model_data_acti |> 
  filter(participant_id!="A324")

bmi_plot<-ggplot(data = model_data_acti_plots, aes(x = BMI)) +
  # Shaded region for "Normal weight"
  geom_histogram(color = "white", fill = "#2c3e50", bins = 30) +
  # Titles and labels
  labs(
    title = NULL,
    x = paste0("BMI (kg/m^2) (N = ",sum(!is.na(model_data_acti_plots$BMI)), ")"),
    y = NULL
  ) +
  # Clean and modern theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

map_plot<-ggplot(data = model_data_acti_plots, aes(x = MAP)) +
  # Shaded region for "Normal weight"
  geom_histogram(color = "white", fill = "#2c3e50", bins = 30) +
  # Titles and labels
  labs(
    title = NULL,
    x = paste0("MAP (mmHg) (N = ",sum(!is.na(model_data_acti_plots$MAP)), ")"),
    y = NULL
  ) +
  # Clean and modern theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

crp_plot<-ggplot(data = model_data_acti_plots |> 
                   filter(crp<10), aes(x = crp)) +
  # Shaded region for "Normal weight"
  geom_histogram(color = "white", fill = "#2c3e50", bins = 30) +
  # Titles and labels
  labs(
    title = NULL,
    x = paste0("CRP (m/L) (N = ",sum(!is.na(model_data_acti |> 
                                              filter(crp<10&crp>0) |> 
                                              select(crp))), ")"),
    y = NULL
  ) +
  # Clean and modern theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

bodyfat_plot<-ggplot(data = model_data_acti_plots, aes(x = body_fat)) +
  # Shaded region for "Normal weight"
  geom_histogram(color = "white", fill = "#2c3e50", bins = 30) +
  # Titles and labels
  labs(
    title = NULL,
    x = paste0("Body fat (%) (N = ",sum(!is.na(model_data_acti_plots$body_fat)), ")"),
    y = NULL
  ) +
  # Clean and modern theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )


waistcircumference_plot<-ggplot(data = model_data_acti_plots, aes(x = waist_circumference)) +
  # Shaded region for "Normal weight"
  geom_histogram(color = "white", fill = "#2c3e50", bins = 30) +
  # Titles and labels
  labs(
    title = NULL,
    x = paste0("Waist circumference (cm) (N = ",sum(!is.na(model_data_acti_plots$waist_circumference)), ")"),
    y = NULL
  ) +
  # Clean and modern theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

glucose_plot<-ggplot(data = model_data_acti_plots, aes(x = glucose)) +
  # Shaded region for "Normal weight"
  geom_histogram(color = "white", fill = "#2c3e50", bins = 30) +
  # Titles and labels
  labs(
    title = NULL,
    x = paste0("Glucose (mg/dL) (N = ",sum(!is.na(model_data_acti_plots$glucose)), ")"),
    y = NULL
  ) +
  # Clean and modern theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

ldl_plot<-ggplot(data = model_data_acti_plots, aes(x = ldl_cholesterol)) +
  # Shaded region for "Normal weight"
  geom_histogram(color = "white", fill = "#2c3e50", bins = 30) +
  # Titles and labels
  labs(
    title = NULL,
    x = paste0("LDL Cholesterol (mg/dL) (N = ",sum(!is.na(model_data_acti_plots$ldl_cholesterol)), ")"),
    y = NULL
  ) +
  # Clean and modern theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

triglycerides_plot<-ggplot(data = model_data_acti_plots, aes(x = triglycerides)) +
  # Shaded region for "Normal weight"
  geom_histogram(color = "white", fill = "#2c3e50", bins = 30) +
  # Titles and labels
  labs(
    title = NULL,
    x = paste0("Triglycerides (mg/dL) (N = ",sum(!is.na(model_data_acti_plots$triglycerides)), ")"),
    y = NULL
  ) +
  # Clean and modern theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )+
  geom_vline(aes(xintercept = 50), linetype = "dashed")

hdl_plot<-ggplot(data = model_data_acti, aes(x = hdl_cholesterol)) +
  # Shaded region for "Normal weight"
  geom_histogram(color = "white", fill = "#2c3e50", bins = 30) +
  # Titles and labels
  labs(
    title = NULL,
    x = paste0("HDL Cholesterol (mg/dL) (N = ",sum(!is.na(model_data_acti$hdl_cholesterol)), ")"),
    y = NULL
  ) +
  # Clean and modern theme
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )+
  geom_vline(aes(xintercept = 20), linetype = "dashed")

metsyn_counts <- model_data_acti |>
  filter(!is.na(metabolic_syndrome_binary)) |>
  count(metabolic_syndrome_binary) |>
  mutate(
    label = factor(metabolic_syndrome_binary, levels = c(0, 1),
                   labels = c("No", "Yes")),
    pct  = n / sum(n) * 100
  )

met_s_pichart <- ggplot(metsyn_counts, aes(x = "", y = n, fill = label)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(pct), "%")),
            position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_manual(values = c("No" = "#4E79A7",
                               "Yes"    = "#D9534F")) +
  labs(title = "Metabolic Syndrome Prevalence (n=100)", fill = NULL) +
  theme_void()

combo_plot_acti_biomarkers <- map_plot + crp_plot + bodyfat_plot + waistcircumference_plot + bmi_plot + glucose_plot + triglycerides_plot + ldl_plot + hdl_plot 



combo_plot_acti_biomarkers + plot_annotation(
  tag_levels = "A"
)+ plot_layout(ncol = 2, guides = "collect")

ggsave("supp_acti_sample_biomarkers.jpeg", path = "/Users/levkolinski/Library/CloudStorage/Box-Box/EEID_Data_public/manuscripts/cardiometabolic_health_food_security/Figures/",
       units="in", width=10, height=14, dpi=350, device = "jpeg", bg = "white")







