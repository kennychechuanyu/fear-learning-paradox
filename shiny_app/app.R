library(shiny)
library(bslib)
library(tidyverse)
library(shinyjs)

source("functions/rw_core.R")

nature_colors <- c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4")

# --- UI COMPONENTS ---

# Author footer component (reusable across all pages)
author_footer <- function() {
  div(
    style = "margin-top: 60px; padding-top: 30px; border-top: 2px solid #E0E0E0; text-align: center; color: #757575;",
    p(style = "margin: 5px 0; font-size: 0.95rem;",
      strong("Kenny Yu"), " | ",
      a(href = "mailto:kenny.yu@kuleuven.be", "kenny.yu@kuleuven.be",
        style = "color: #1976D2; text-decoration: none;")
    ),
    p(style = "margin: 5px 0; font-size: 0.9rem;",
      "Quantitative Psychology and Individual Differences, KU Leuven"
    )
  )
}

ui <- page_navbar(
  title = "The Fear Learning Reliability Paradox",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  id = "main_nav",
  header = shinyjs::useShinyjs(),

  # -- MAIN PAGE: THE PARADOX --
  nav_panel(
    title = "The Paradox",
    icon = icon("home"),
    value = "home",

    div(style = "max-width: 1200px; margin: auto; padding: 40px;",
      h1("The Reliability Paradox", style = "text-align: center; color: #2C3E50; margin-bottom: 20px;"),

      # Author info on main page
      div(style = "text-align: center; margin-bottom: 30px;",
        div(style = "display: inline-block; padding: 20px; background: #F5F5F5; border-radius: 10px;",
          p(style = "margin: 5px 0; font-size: 1.05rem; color: #424242;",
            strong("Created by Kenny Yu"), tags$br(),
            a(href = "mailto:kenny.yu@kuleuven.be", "kenny.yu@kuleuven.be",
              style = "color: #1976D2; text-decoration: none;"), tags$br(),
            span(style = "font-size: 0.95rem; color: #666;",
              "Quantitative Psychology and Individual Differences, KU Leuven")
          )
        )
      ),

      div(style = "background: #fff3cd; padding: 25px; border-radius: 10px; margin-bottom: 40px; border-left: 5px solid #ffc107;",
        h3("The Puzzle", style = "color: #856404; margin-top: 0;"),
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50;",
          "A fundamental puzzle emerges in psychological research: ",
          strong("individual behavior shows poor test-retest reliability"),
          ", yet ",
          strong("group-level patterns replicate robustly"),
          ". This paradox appears across many domains, including fear learning research."
        ),
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50; margin-bottom: 0;",
          strong("This reflects the problem of using behavioral markers directly."),
          " Fear conditioning research seeks reliable markers to guide clinical translation for anxiety treatment, ",
          "but behavior emerges from interactions between ", strong("latent psychological processes"),
          " (like learning mechanisms) and ", strong("experimental context"),
          " (like shock sequences). But first: ",
          actionLink("goto_measuring", "what are behavioral vs cognitive markers?",
                    style = "color: #1976D2; text-decoration: underline; font-weight: bold;"),
          " I then illustrate three sources that explain this paradox..."
        )
      ),

      fluidRow(
        column(3,
          wellPanel(
            h4("Simulate the Paradox"),
            sliderInput("n_people_home", "Number of People:", 8, 30, 15, 1),
            sliderInput("n_trials_home", "Trials per Session:", 15, 40, 25, 5),
            sliderInput("noise_home", "Behavioral Noise:", 2, 8, 4, 1),
            hr(),
            actionButton("simulate_home", "Run Simulation", class = "btn-warning btn-lg btn-block", icon = icon("sync")),
            hr(),
            div(style = "background: #f8f9fa; padding: 15px; border-radius: 5px;",
              uiOutput("home_stats")
            )
          )
        ),

        column(9,
          h3("Individual Patterns: Inconsistent", style = "color: #2C3E50; text-align: center; margin-bottom: 20px;"),
          p("Each person tested twice. Different shock patterns each session. Lines cross everywhere—individuals are unreliable.",
            style = "text-align: center; color: #666; margin-bottom: 20px;"),
          plotOutput("plot_home_individual", height = "350px"),

          div(style = "margin-top: 40px;"),
          h3("Group Average: Perfectly Stable", style = "color: #2C3E50; text-align: center; margin-bottom: 20px;"),
          p("Despite individual chaos, the group average replicates almost identically across sessions.",
            style = "text-align: center; color: #666; margin-bottom: 20px;"),
          plotOutput("plot_home_group", height = "350px")
        )
      ),

      div(style = "margin-top: 50px; background: #e3f2fd; padding: 30px; border-radius: 10px;",
        h3("Why Does This Happen? Three Sources", style = "color: #1976D2; text-align: center; margin-top: 0;"),
        fluidRow(
          column(4,
            div(style = "background: white; padding: 20px; border-radius: 8px; margin: 10px; min-height: 220px; border-top: 4px solid #00A087;",
              h4("Source 1", style = "color: #00A087;"),
              h5("Context-Dependent Behavior"),
              p(style = "font-size: 0.95em;", "Behavioral responses depend on both mechanism AND context. Same mechanism can produce different behaviors; different mechanisms can produce same behaviors."),
              actionButton("goto_source1", "Explore →", class = "btn-success", style = "width: 100%; margin-top: 10px;")
            )
          ),
          column(4,
            div(style = "background: white; padding: 20px; border-radius: 8px; margin: 10px; min-height: 220px; border-top: 4px solid #4DBBD5;",
              h4("Source 2", style = "color: #4DBBD5;"),
              h5("Multiple Latent Processes"),
              p(style = "font-size: 0.95em;", "Behavior is generated by multiple latent processes. As an example: learning rate + memory retention. Different process combinations can produce similar behavioral patterns."),
              actionButton("goto_source2", "Explore →", class = "btn-primary", style = "width: 100%; margin-top: 10px;")
            )
          ),
          column(4,
            div(style = "background: white; padding: 20px; border-radius: 8px; margin: 10px; min-height: 220px; border-top: 4px solid #E64B35;",
              h4("Source 3", style = "color: #E64B35;"),
              h5("Model Misspecification"),
              p(style = "font-size: 0.95em;", "Using the wrong learning model produces invalid cognitive markers. Same learning rate, same shocks, but different learning mechanisms (RW vs PH) produce different behaviors."),
              actionButton("goto_source3", "Explore →", class = "btn-danger", style = "width: 100%; margin-top: 10px;")
            )
          )
        )
      ),

      # Footer
      author_footer()
    )
  ),

  # -- MEASURING LEARNING: BEHAVIORAL VS COGNITIVE MARKERS --
  nav_panel(
    title = "Measuring Learning",
    icon = icon("ruler"),
    value = "measuring",

    div(style = "max-width: 1200px; margin: auto; padding: 40px;",
      h1("Two Ways to Measure Learning", style = "text-align: center; color: #2C3E50; margin-bottom: 40px;"),

      div(style = "background: #e8f5e9; padding: 25px; border-radius: 10px; margin-bottom: 40px; border-left: 5px solid #00A087;",
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50; margin-bottom: 10px;",
          "To develop ", strong("personalized treatments for anxiety disorders"),
          ", we need reliable markers to guide interventions. ",
          "When we observe someone learning, we can extract two types of markers:"
        )
      ),

      fluidRow(
        column(3,
          wellPanel(style = "background: #f8f9fa;",
            h5("Simulate One Learner", style = "margin-top: 0;"),
            sliderInput("alpha_measuring", "True Learning Rate (α):", 0.2, 0.8, 0.5, 0.1),
            sliderInput("n_trials_measuring", "Number of Trials:", 15, 40, 25, 5),
            actionButton("simulate_measuring", "Generate Learning Curve",
                        class = "btn-primary btn-lg btn-block", icon = icon("play")),
            hr(),
            div(style = "background: #fff3cd; padding: 12px; border-radius: 5px;",
              p(style = "font-size: 0.9em; margin: 0; color: #856404;",
                strong("Try this: "), "Change α and see how both behavioral and cognitive markers change.")
            )
          )
        ),

        column(9,
          h4("Observed Learning Curve", style = "color: #2C3E50; margin-bottom: 15px;"),
          plotOutput("plot_measuring", height = "350px"),

          hr(),

          fluidRow(
            column(6,
              div(style = "background: #fff3cd; padding: 20px; border-radius: 8px; border-top: 4px solid #ffc107;",
                h5("Behavioral Markers", style = "color: #856404; margin-top: 0;"),
                uiOutput("behavioral_indices")
              )
            ),
            column(6,
              div(style = "background: #e3f2fd; padding: 20px; border-radius: 8px; border-top: 4px solid #1976D2;",
                h5("Cognitive Marker", style = "color: #1976D2; margin-top: 0;"),
                uiOutput("cognitive_indices")
              )
            )
          )
        )
      ),

      hr(style = "margin: 40px 0;"),

      h3("What Do These Measure?", style = "text-align: center; color: #2C3E50; margin-bottom: 30px;"),

      fluidRow(
        column(6,
          div(style = "background: #fff3cd; padding: 25px; border-radius: 10px; min-height: 400px; border-top: 5px solid #ffc107;",
            h3("Behavioral Markers", style = "color: #856404; margin-top: 0;"),
            h5("Direct observation of behavior", style = "color: #666;"),

            tags$ul(style = "font-size: 1em; line-height: 1.8;",
              tags$li(strong("Learning slope:"), " Rate of change across trials"),
              tags$li(strong("First→Last difference:"), " Overall improvement"),
              tags$li(strong("Mean response:"), " Average performance"),
              tags$li(strong("Final trial response:"), " End-state performance")
            ),

            div(style = "background: white; padding: 15px; border-radius: 5px; margin-top: 20px;",
              h5("Assumption:", style = "color: #856404; margin-top: 0;"),
              p(style = "font-size: 0.95em; color: #666; margin: 0;",
                "These statistics ", strong("directly reflect"), " the learning process. ",
                "We can compare individuals using these descriptive measures."
              )
            ),

            div(style = "background: #f8d7da; padding: 12px; border-radius: 5px; margin-top: 15px; border-left: 3px solid #dc3545;",
              p(style = "font-size: 0.9em; color: #721c24; margin: 0;",
                strong("Problem: "), "Context-dependent! Same mechanism can produce different slopes/differences in different conditions."
              )
            )
          )
        ),

        column(6,
          div(style = "background: #e3f2fd; padding: 25px; border-radius: 10px; min-height: 400px; border-top: 5px solid #1976D2;",
            h3("Cognitive Markers", style = "color: #1976D2; margin-top: 0;"),
            h5("Model-based parameter estimation", style = "color: #666;"),

            div(style = "background: white; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
              p(style = "font-size: 0.95em; margin: 0; font-family: monospace;",
                "V(t+1) = V(t) + ", strong("α"), " × [outcome - V(t)]"
              )
            ),

            tags$ul(style = "font-size: 1em; line-height: 1.8;",
              tags$li(strong("Learning rate (α):"), " Speed of updating beliefs"),
              tags$li("Fit computational model to observed behavior"),
              tags$li("Extract latent parameters"),
              tags$li("Assumes behavior is ", em("generated by"), " this process")
            ),

            div(style = "background: white; padding: 15px; border-radius: 5px; margin-top: 20px;",
              h5("Assumption:", style = "color: #1976D2; margin-top: 0;"),
              p(style = "font-size: 0.95em; color: #666; margin: 0;",
                "Behavior is ", strong("generated by"), " a latent computational process. ",
                "Parameters reveal the ", em("mechanism"), " underlying behavior."
              )
            ),

            div(style = "background: #f8d7da; padding: 12px; border-radius: 5px; margin-top: 15px; border-left: 3px solid #dc3545;",
              p(style = "font-size: 0.9em; color: #721c24; margin: 0;",
                strong("Problem: "), "Model-dependent! Wrong model = wrong parameters. Plus: multiple processes can generate behavior."
              )
            )
          )
        )
      ),

      div(style = "background: #fff3cd; padding: 25px; border-radius: 10px; margin-top: 40px; border-left: 5px solid #ffc107;",
        h3("The Key Question", style = "color: #856404; margin-top: 0;"),
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50;",
          "Both approaches have ", strong("implicit assumptions"), " about what they're measuring. ",
          "Behavioral markers assume behavior directly reflects learning. ",
          "Cognitive markers assume a specific computational model. ",
          "But what if these assumptions are wrong?"
        ),
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50; margin-bottom: 0;",
          strong("For personalized medicine in anxiety disorders, we need reliable markers."),
          " Let's explore three sources of unreliability..."
        )
      ),

      div(style = "text-align: center; margin-top: 40px;",
        actionButton("goto_source1_from_measuring", "Explore Source 1 →",
                    class = "btn-success btn-lg", style = "margin: 10px;"),
        actionButton("goto_home_from_measuring", "← Back to Paradox",
                    class = "btn-secondary btn-lg", style = "margin: 10px;")
      ),

      # Footer
      author_footer()
    )
  ),

  # -- SOURCE 1: CONTEXT-DEPENDENT BEHAVIOR --
  nav_panel(
    title = "Source 1: Context Sensitivity",
    icon = icon("exchange-alt"),
    value = "source1",

    div(style = "max-width: 1400px; margin: auto; padding: 20px;",

      div(style = "background: #e8f5e9; padding: 25px; border-radius: 10px; margin-bottom: 30px; border-left: 5px solid #00A087;",
        h2("Source 1: Behavioral Markers Are Context-Sensitive", style = "color: #00A087; margin-top: 0;"),
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50;",
          strong("The Problem: "), "Behavior doesn't provide a pure readout of learning mechanisms. ",
          "The same mechanism produces different behaviors in different contexts, and different mechanisms can produce similar behaviors when contexts compensate."
        ),
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50; margin-bottom: 0;",
          strong("Implication: "), "Simple behavioral markers (mean response, difference scores) are fundamentally ambiguous. ",
          "We cannot directly infer mechanisms from behavior alone."
        )
      ),

      h3("Demonstration 1A: Same Mechanism → Different Behaviors", style = "color: #2C3E50; margin-bottom: 20px;"),
      p("One person, same learning rate (α = 0.5), but different shock sequences across two conditions.",
        style = "color: #666; font-size: 1.05em; margin-bottom: 25px;"),

      fluidRow(
        column(3,
          wellPanel(style = "background: #f8f9fa;",
            h5("Settings", style = "margin-top: 0;"),
            sliderInput("alpha_s1a", "Learning Rate (α):", 0.1, 0.9, 0.5, 0.05),
            hr(),
            h6("Condition 1:"),
            selectInput("timing_s1a_c1", "Shock Sequence:",
                       choices = c("Early" = "early", "Late" = "late", "Distributed" = "distributed"),
                       selected = "early"),
            h6("Condition 2:", style = "margin-top: 15px;"),
            selectInput("timing_s1a_c2", "Shock Sequence:",
                       choices = c("Early" = "early", "Late" = "late", "Distributed" = "distributed"),
                       selected = "late"),
            hr(),
            sliderInput("n_trials_s1a", "Trials per Condition:", 15, 40, 25, 5),
            sliderInput("n_shocks_s1a", "Number of Shocks:", 5, 25, 15, 1),
            actionButton("simulate_s1a", "Run Demo 1A", class = "btn-success btn-lg btn-block", icon = icon("play"))
          )
        ),
        column(9,
          plotOutput("plot_s1a", height = "450px"),
          div(style = "background: #fff3cd; padding: 15px; border-radius: 5px; margin-top: 15px; border-left: 3px solid #ffc107;",
            p(style = "margin: 0; font-size: 1em; color: #856404;",
              strong("Key Insight: "), "The learning mechanism (α = ", textOutput("alpha_s1a_display", inline = TRUE),
              ") never changed, yet the behavioral curves look completely different. ",
              "Context matters! Behavior is not a pure readout of the mechanism.")
          )
        )
      ),

      hr(style = "margin-top: 50px; margin-bottom: 50px;"),

      h3("Demonstration 1B: Different Mechanisms → Same Behavior", style = "color: #2C3E50; margin-bottom: 20px;"),
      p("Two people with very different learning rates, but compensating shock sequences produce similar behavioral outcomes.",
        style = "color: #666; font-size: 1.05em; margin-bottom: 25px;"),

      fluidRow(
        column(3,
          wellPanel(style = "background: #f8f9fa;",
            h5("Settings", style = "margin-top: 0;"),
            h6("Person A (Fast Learner)", style = "color: #4DBBD5;"),
            sliderInput("alpha_A_s1b", "Learning Rate:", 0.3, 0.9, 0.75, 0.05),
            selectInput("timing_A_s1b", "Shock Sequence:",
                       choices = c("Early" = "early", "Distributed" = "distributed", "Late" = "late"),
                       selected = "distributed"),
            hr(),
            h6("Person B (Slow Learner)", style = "color: #E64B35;"),
            sliderInput("alpha_B_s1b", "Learning Rate:", 0.1, 0.7, 0.25, 0.05),
            selectInput("timing_B_s1b", "Shock Sequence:",
                       choices = c("Early" = "early", "Distributed" = "distributed", "Late" = "late"),
                       selected = "early"),
            hr(),
            sliderInput("n_trials_s1b", "Trials:", 20, 50, 30, 5),
            sliderInput("n_shocks_s1b", "Shocks:", 5, 30, 15, 1),
            actionButton("simulate_s1b", "Run Demo 1B", class = "btn-success btn-lg btn-block", icon = icon("play"))
          )
        ),
        column(9,
          plotOutput("plot_s1b", height = "450px"),
          div(style = "background: #f8f9fa; padding: 15px; border-radius: 5px; margin-top: 15px;",
            h5("Behavioral Similarity Despite Large Difference in Learning Rate"),
            tableOutput("stats_s1b")
          ),
          div(style = "background: #fff3cd; padding: 15px; border-radius: 5px; margin-top: 15px; border-left: 3px solid #ffc107;",
            p(style = "margin: 0; font-size: 1em; color: #856404;",
              strong("Key Insight: "), "When behavioral statistics are similar despite very different learning rates, ",
              "we cannot infer mechanisms from behavior. Different mechanisms can produce the same behavioral output.")
          )
        )
      ),

      # Footer
      author_footer()
    )
  ),

  # -- SOURCE 2: MULTIPLE LATENT PROCESSES --
  nav_panel(
    title = "Source 2: Multiple Processes",
    icon = icon("project-diagram"),
    value = "source2",

    div(style = "max-width: 1200px; margin: auto; padding: 20px;",

      div(style = "background: #e3f2fd; padding: 25px; border-radius: 10px; margin-bottom: 30px; border-left: 5px solid #4DBBD5;",
        h2("Source 2: Multiple Latent Processes", style = "color: #4DBBD5; margin-top: 0;"),
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50;",
          strong("The Problem: "), "Behavior is generated by ", strong("multiple latent processes"), ". ",
          "As an example, we demonstrate two processes: ",
          "(1) learning rate (α) during the session, and (2) memory retention between sessions."
        ),
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50;",
          strong("Memory retention controls Session 2 starting point:"), br(),
          "• Memory = 0 → S2 starts fresh (same as S1 Trial 1)", br(),
          "• Memory = 1 → S2 starts where S1 ended (full carry-over)", br(),
          "• Memory = 0.5 → S2 starts halfway between naive and S1 end"
        ),
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50; margin-bottom: 0;",
          "Different (α, memory) combinations can produce ", strong("similar behavioral patterns"), ". ",
          "Example: high α + low memory ≈ low α + high memory. This makes it impossible to infer individual parameters from behavior alone."
        )
      ),

      fluidRow(
        column(3,
          wellPanel(
            h4("Learner Parameters"),
            hr(),
            h5("Fast Learner", style = "color: #0072B2; font-weight: bold;"),
            sliderInput("alpha_fast", "Learning rate (α):", 0.3, 0.9, 0.7, 0.05),
            sliderInput("memory_fast", "Memory retention:", 0.0, 1.0, 0.5, 0.1),
            p(style = "font-size: 0.85em; color: #666; margin-top: -10px;",
              "0=fresh start, 1=full carry-over"),
            hr(),
            h5("Slow Learner", style = "color: #D55E00; font-weight: bold;"),
            sliderInput("alpha_slow", "Learning rate (α):", 0.1, 0.5, 0.25, 0.05),
            sliderInput("memory_slow", "Memory retention:", 0.0, 1.0, 1.0, 0.1),
            p(style = "font-size: 0.85em; color: #666; margin-top: -10px;",
              "0=fresh start, 1=full carry-over"),
            hr(),
            sliderInput("n_trials_s2", "Trials per Session:", 8, 15, 10, 1),
            hr(),
            actionButton("simulate_s2", "Run Simulation", class = "btn-primary btn-lg btn-block", icon = icon("sync")),
            hr(),
            div(style = "background: #f8f9fa; padding: 12px; border-radius: 5px;",
              uiOutput("stats_s2")
            ),
            div(style = "background: #fff3cd; padding: 12px; border-radius: 5px; margin-top: 15px; border-left: 3px solid #ffc107;",
              p(style = "font-size: 0.9em; margin: 0; color: #856404;",
                strong("Try this: "), "Set fast learner memory = 0.5 and slow learner memory = 1.0. Can different (α, memory) produce similar S2 starts?")
            )
          )
        ),

        column(9,
          h3("Carry-Over Between Sessions", style = "color: #2C3E50; margin-bottom: 15px;"),
          p(style = "font-size: 1.05em; color: #666; margin-bottom: 20px;",
            "Notice: ", strong("Both learners start Session 2 much higher than Session 1"),
            " (Trial 1 is circled). This carry-over effect happens regardless of learning speed."),
          plotOutput("plot_s2_carryover", height = "500px"),

          hr(style = "margin: 30px 0;"),

          h3("Session Comparison", style = "color: #2C3E50; margin-bottom: 15px;"),
          p(style = "font-size: 1.05em; color: #666; margin-bottom: 20px;",
            "When we average across individuals (Fast + Slow), ", strong("Session 1 and Session 2 can look similar"),
            " despite different underlying (α, memory) combinations."),
          plotOutput("plot_s2_averaged", height = "350px")
        )
      ),

      # Footer
      author_footer()
    )
  ),

  # -- SOURCE 3: MODEL MISSPECIFICATION --
  nav_panel(
    title = "Source 3: Model Misspecification",
    icon = icon("exclamation-triangle"),
    value = "source3",

    div(style = "max-width: 1400px; margin: auto; padding: 20px;",

      div(style = "background: #ffebee; padding: 25px; border-radius: 10px; margin-bottom: 30px; border-left: 5px solid #E64B35;",
        h2("Source 3: Using the Wrong Learning Model", style = "color: #E64B35; margin-top: 0;"),
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50;",
          strong("The Problem: "), "Even if we measure behavior perfectly, we can get invalid cognitive markers ",
          "if we assume the wrong learning mechanism. Different computational models (RW vs Pearce-Hall) ",
          "represent fundamentally different learning processes."
        ),
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50;",
          strong("RW Model: "), "Fixed learning rate (α) - you always learn the same amount from prediction errors.", br(),
          strong("Pearce-Hall Model: "), "Dynamic attention - learning rate increases after surprising events and decreases when outcomes become predictable."
        ),
        p(style = "font-size: 1.1em; line-height: 1.7; color: #2C3E50; margin-bottom: 0;",
          strong("Implication: "), "Two people with the ", strong("same initial α"), " and experiencing the ",
          strong("same shock sequence"), " will show ", strong("different behavioral patterns"),
          " depending on which learning mechanism they use. Fitting the wrong model produces misleading cognitive markers."
        )
      ),

      h3("Demonstration: Same α, Same Shocks, Different Learning Mechanisms", style = "color: #2C3E50; margin-bottom: 20px;"),
      p("Two people with identical initial learning rate but different learning mechanisms.",
        style = "color: #666; font-size: 1.05em; margin-bottom: 25px;"),

      fluidRow(
        column(3,
          wellPanel(style = "background: #f8f9fa;",
            h5("Settings", style = "margin-top: 0;"),
            sliderInput("alpha_s3", "Initial Learning Rate (α):", 0.3, 0.9, 0.6, 0.05),
            p(style = "font-size: 0.85em; color: #666; margin-top: -10px;",
              "Same for both RW and PH learners"),
            hr(),
            sliderInput("reinforce_rate_s3", "Reinforcement Rate:",
                       min = 0.3, max = 0.8, value = 0.5, step = 0.1),
            p(style = "font-size: 0.85em; color: #666; margin-top: -10px;",
              "Proportion of trials with shocks (same for both learners)"),
            hr(),
            sliderInput("kappa_s3", "PH Attention Decay (κ):", 0.3, 0.9, 0.8, 0.1),
            p(style = "font-size: 0.85em; color: #666; margin-top: -10px;",
              "Higher κ = attention responds more to surprises"),
            hr(),
            sliderInput("n_trials_s3", "Number of Trials:", 20, 50, 30, 5),
            hr(),
            actionButton("simulate_s3", "Run Demo", class = "btn-danger btn-lg btn-block", icon = icon("play")),
            hr(),
            div(style = "background: #fff3cd; padding: 12px; border-radius: 5px; border-left: 3px solid #ffc107;",
              p(style = "font-size: 0.9em; margin: 0; color: #856404;",
                strong("Key insight: "), "If you assume RW (fixed learning) but the person uses PH (attention-weighted learning), you get the wrong cognitive marker. Lower reinforcement rates (40-60%) create more uncertainty, keeping PH attention high while RW learns intermediate values—showing that wrong model = wrong assumptions about the underlying process.")
            )
          )
        ),

        column(9,
          h4("Learning Curves: RW vs Pearce-Hall", style = "color: #2C3E50; margin-bottom: 15px;"),
          plotOutput("plot_s3_learning", height = "400px"),

          hr(style = "margin: 25px 0;"),

          h4("Attention Trajectory (PH Model Only)", style = "color: #2C3E50; margin-bottom: 15px;"),
          p(style = "font-size: 1em; color: #666; margin-bottom: 15px;",
            "RW model uses constant α = ", textOutput("alpha_s3_display", inline = TRUE),
            " throughout. PH model's attention changes dynamically:"),
          plotOutput("plot_s3_attention", height = "300px"),

          div(style = "background: #ffebee; padding: 15px; border-radius: 5px; margin-top: 20px; border-left: 3px solid #E64B35;",
            p(style = "margin: 0; font-size: 1em; color: #c62828;",
              strong("Key Insight: "), "Same α, same shocks, but different learning curves! ",
              "If we fit a simple RW model to a PH learner's data, the recovered α parameter won't reflect their true learning mechanism. ",
              "The cognitive marker becomes invalid.")
          )
        )
      ),

      # Footer
      author_footer()
    )
  )
)

# --- SERVER ---

server <- function(input, output, session) {

  source1a_autorun <- reactiveVal(TRUE)
  source1b_autorun <- reactiveVal(TRUE)
  source2_autorun <- reactiveVal(TRUE)
  source3_autorun <- reactiveVal(TRUE)
  home_autorun <- reactiveVal(TRUE)

  # Navigation handlers
  observeEvent(input$goto_measuring, {
    updateNavbarPage(session, "main_nav", selected = "measuring")
  })

  observeEvent(input$goto_home_from_measuring, {
    updateNavbarPage(session, "main_nav", selected = "home")
  })

  observeEvent(input$goto_source1_from_measuring, {
    updateNavbarPage(session, "main_nav", selected = "source1")
    if (source1a_autorun()) {
      shinyjs::delay(100, click("simulate_s1a"))
      source1a_autorun(FALSE)
    }
  })

  observeEvent(input$goto_source1, {
    updateNavbarPage(session, "main_nav", selected = "source1")
    if (source1a_autorun()) {
      shinyjs::delay(100, click("simulate_s1a"))
      source1a_autorun(FALSE)
    }
  })

  observeEvent(input$goto_source2, {
    updateNavbarPage(session, "main_nav", selected = "source2")
    if (source2_autorun()) {
      shinyjs::delay(100, click("simulate_s2"))
      source2_autorun(FALSE)
    }
  })

  observeEvent(input$goto_source3, {
    updateNavbarPage(session, "main_nav", selected = "source3")
    if (source3_autorun()) {
      shinyjs::delay(100, click("simulate_s3"))
      source3_autorun(FALSE)
    }
  })

  observeEvent(input$main_nav, {
    if (input$main_nav == "home" && home_autorun()) {
      shinyjs::delay(300, click("simulate_home"))
      home_autorun(FALSE)
    }
    if (input$main_nav == "source1" && source1a_autorun()) {
      shinyjs::delay(300, click("simulate_s1a"))
      source1a_autorun(FALSE)
    }
    if (input$main_nav == "source2" && source2_autorun()) {
      shinyjs::delay(300, click("simulate_s2"))
      source2_autorun(FALSE)
    }
    if (input$main_nav == "source3" && source3_autorun()) {
      shinyjs::delay(300, click("simulate_s3"))
      source3_autorun(FALSE)
    }
  })

  # Auto-run on app start
  observe({
    if (home_autorun()) {
      shinyjs::delay(500, click("simulate_home"))
      home_autorun(FALSE)
    }
  })

  # -- MEASURING LEARNING: INTERACTIVE DEMO --

  data_measuring <- reactive({
    req(input$simulate_measuring)

    alpha <- input$alpha_measuring
    n_trials <- input$n_trials_measuring

    # Generate random shock sequence (60% reinforcement)
    n_shocks <- round(n_trials * 0.6)
    shocks <- sort(sample(1:n_trials, n_shocks))

    # Simulate learning
    d <- simulate_rw_session(alpha, n_trials, shocks, sigma = 3)
    d$icon <- ifelse(d$shock, "⚡", "☺")

    d
  }) %>% bindEvent(input$simulate_measuring)

  output$plot_measuring <- renderPlot({
    req(data_measuring())
    d <- data_measuring()

    ggplot(d, aes(x = trial, y = response)) +
      geom_line(linewidth = 1.5, color = "#1976D2", alpha = 0.8) +
      geom_point(size = 3, color = "#1976D2", alpha = 0.7) +
      geom_text(aes(label = icon), size = 6, vjust = -1.5, family = "sans") +
      # Add first and last points highlighted
      geom_point(data = d[c(1, nrow(d)), ], size = 5, color = "#E64B35") +
      # Add mean line
      geom_hline(aes(yintercept = mean(response)),
                linetype = "dashed", color = "#00A087", linewidth = 1) +
      annotate("text", x = max(d$trial) * 0.9, y = mean(d$response) + 5,
              label = "Mean", color = "#00A087", fontface = "bold") +
      scale_y_continuous(limits = c(-5, 115), breaks = seq(0, 100, 25)) +
      labs(
        title = sprintf("One Person Learning (True α = %.1f)", input$alpha_measuring),
        subtitle = "⚡ = Shock trial   ☺ = Safe trial",
        x = "Trial",
        y = "Response"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "#666"),
        panel.grid.minor = element_blank()
      )
  })

  output$behavioral_indices <- renderUI({
    req(data_measuring())
    d <- data_measuring()

    # Calculate behavioral markers
    markers <- calculate_behavioral_indices(d$response)

    tagList(
      p(style = "margin: 8px 0;",
        strong("Mean response: "), sprintf("%.1f", markers$mean)),
      p(style = "margin: 8px 0;",
        strong("Learning slope: "), sprintf("%.2f", markers$slope)),
      p(style = "margin: 8px 0;",
        strong("First→Last: "), sprintf("%.1f", markers$first_last_diff)),
      p(style = "margin: 8px 0;",
        strong("Final trial: "), sprintf("%.1f", markers$final_trial)),
      hr(),
      p(style = "font-size: 0.85em; color: #666; font-style: italic;",
        "Calculated directly from observed responses")
    )
  })

  output$cognitive_indices <- renderUI({
    req(data_measuring())
    d <- data_measuring()

    # Recover learning rate using MLE
    shocks <- which(d$shock)
    recovered_alpha <- recover_alpha_mle(
      responses = d$response,
      shock_trials = shocks,
      n_trials = nrow(d)
    )

    true_alpha <- input$alpha_measuring

    tagList(
      p(style = "margin: 8px 0;",
        strong("True α: "), sprintf("%.2f", true_alpha)),
      p(style = "margin: 8px 0;",
        strong("Recovered α: "), sprintf("%.2f", recovered_alpha)),
      p(style = "margin: 8px 0;",
        strong("Error: "), sprintf("%.2f", abs(recovered_alpha - true_alpha))),
      hr(),
      p(style = "font-size: 0.85em; color: #666; font-style: italic;",
        "Estimated by fitting RW model to data")
    )
  })

  # -- HOME PAGE: THE PARADOX --

  data_home <- reactive({
    req(input$simulate_home)

    n_people <- input$n_people_home
    n_trials <- input$n_trials_home
    noise <- input$noise_home

    all_data <- list()

    colors <- rep(c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4",
                    "#DC0000", "#7876B1", "#EE0000", "#008B45", "#631879", "#008280"),
                  length.out = n_people)

    for (i in 1:n_people) {
      true_alpha <- rnorm(1, 0.5, 0.12)
      true_alpha <- pmax(0.2, pmin(0.8, true_alpha))

      n_shocks <- round(n_trials * 0.65)
      shocks_s1 <- sort(sample(1:n_trials, n_shocks))
      shocks_s2 <- sort(sample(1:n_trials, n_shocks))

      s1 <- simulate_rw_session(true_alpha, n_trials, shocks_s1, sigma = noise)
      s1$person <- paste0("P", i)
      s1$session <- "Session1"
      s1$color <- colors[i]
      s1$alpha <- true_alpha

      s2 <- simulate_rw_session(true_alpha, n_trials, shocks_s2, sigma = noise)
      s2$person <- paste0("P", i)
      s2$session <- "Session2"
      s2$color <- colors[i]
      s2$alpha <- true_alpha

      all_data <- c(all_data, list(s1, s2))
    }

    bind_rows(all_data)
  }) %>% bindEvent(input$simulate_home)

  output$plot_home_individual <- renderPlot({
    req(data_home())
    d <- data_home()

    # Create matched pairs for visualization
    d_wide <- d %>%
      select(person, session, trial, response) %>%
      pivot_wider(names_from = session, values_from = response, values_fn = list) %>%
      unnest(cols = c(Session1, Session2))

    ggplot(d_wide, aes(x = Session1, y = Session2, group = person)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", linewidth = 1) +
      geom_point(alpha = 0.3, size = 2, color = "#E64B35") +
      geom_smooth(method = "lm", se = FALSE, alpha = 0.5, linewidth = 0.8) +
      labs(x = "Response in Session 1", y = "Response in Session 2",
           title = "Individual responses are inconsistent (lines diverge from diagonal)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
            panel.grid.minor = element_blank())
  })

  output$plot_home_group <- renderPlot({
    req(data_home())
    d <- data_home()

    avg <- d %>%
      group_by(session, trial) %>%
      summarize(mean_resp = mean(response, na.rm = TRUE),
                se = sd(response, na.rm = TRUE) / sqrt(n()),
                .groups = "drop")

    ggplot(avg, aes(x = trial, y = mean_resp, color = session, fill = session)) +
      geom_ribbon(aes(ymin = mean_resp - se, ymax = mean_resp + se), alpha = 0.2, color = NA) +
      geom_line(linewidth = 2.5) +
      scale_color_manual(values = c("Session1" = "#4DBBD5", "Session2" = "#E64B35"),
                        name = "", labels = c("Session 1", "Session 2")) +
      scale_fill_manual(values = c("Session1" = "#4DBBD5", "Session2" = "#E64B35"),
                       name = "", labels = c("Session 1", "Session 2")) +
      labs(x = "Trial", y = "Mean Response",
           title = "Group averages overlap almost perfectly") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
            panel.grid.minor = element_blank())
  })

  output$home_stats <- renderUI({
    req(data_home())
    d <- data_home()

    # Individual-level reliability: correlate per-person mean responses across sessions
    person_means <- d %>%
      group_by(person, session) %>%
      summarize(mean_resp = mean(response, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = session, values_from = mean_resp)

    ind_cor <- cor(person_means$Session1, person_means$Session2, use = "complete.obs")

    # Group-level reliability: correlate trial-averaged responses across sessions
    avg <- d %>%
      group_by(session, trial) %>%
      summarize(m = mean(response, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = session, values_from = m)

    grp_cor <- cor(avg$Session1, avg$Session2)

    tagList(
      h5("The Paradox:", style = "font-weight: bold; color: #856404;"),
      p(sprintf("Individual-level test-retest: r = %.2f", ind_cor),
        style = "font-size: 1em; margin: 8px 0; color: #E64B35; font-weight: bold;"),
      p("→ Poor reliability (per-person mean response)", style = "font-size: 0.9em; margin: 3px 0 15px 15px; color: #666;"),
      p(sprintf("Group-level test-retest: r = %.2f", grp_cor),
        style = "font-size: 1em; margin: 8px 0; color: #00A087; font-weight: bold;"),
      p("→ Excellent reliability (trial-averaged across participants)", style = "font-size: 0.9em; margin: 3px 0 0 15px; color: #666;")
    )
  })

  # -- SOURCE 1A: SAME MECHANISM, DIFFERENT BEHAVIOR --

  generate_shock_timing <- function(timing, n_trials, n_shocks = NULL) {
    if (timing == "partial") {
      # 50% partial reinforcement - random shocks on half the trials
      n_shocks_partial <- round(n_trials * 0.5)
      sort(sample(1:n_trials, n_shocks_partial))
    } else if (timing == "early") {
      sample(1:floor(n_trials * 0.4), min(n_shocks, floor(n_trials * 0.4)))
    } else if (timing == "late") {
      sample(ceiling(n_trials * 0.6):n_trials, min(n_shocks, n_trials - ceiling(n_trials * 0.6) + 1))
    } else if (timing == "distributed") {
      round(seq(1, n_trials, length.out = n_shocks))
    } else {
      sample(1:n_trials, min(n_shocks, n_trials))
    }
  }

  data_s1a <- reactive({
    req(input$simulate_s1a)

    alpha <- input$alpha_s1a
    n_trials <- input$n_trials_s1a
    n_shocks <- input$n_shocks_s1a

    shocks_c1 <- generate_shock_timing(input$timing_s1a_c1, n_trials, n_shocks)
    shocks_c2 <- generate_shock_timing(input$timing_s1a_c2, n_trials, n_shocks)

    d1 <- simulate_rw_session(alpha, n_trials, shocks_c1, sigma = 2.5)
    d1$condition <- sprintf("Condition 1: %s sequence", input$timing_s1a_c1)
    d1$icon <- ifelse(d1$shock, "⚡", "☺")

    d2 <- simulate_rw_session(alpha, n_trials, shocks_c2, sigma = 2.5)
    d2$condition <- sprintf("Condition 2: %s sequence", input$timing_s1a_c2)
    d2$icon <- ifelse(d2$shock, "⚡", "☺")

    rbind(d1, d2)
  }) %>% bindEvent(input$simulate_s1a)

  output$plot_s1a <- renderPlot({
    req(data_s1a())
    d <- data_s1a()

    ggplot(d, aes(x = trial, y = response, color = condition)) +
      geom_line(linewidth = 1.8) +
      geom_text(aes(label = icon), size = 7, family = "sans") +
      scale_color_manual(values = c("#4DBBD5", "#E64B35"), name = "") +
      labs(title = sprintf("Same Person, Same Learning Rate (α = %.2f)", input$alpha_s1a),
           subtitle = "⚡ = Shock trial   ☺ = Safe trial",
           x = "Trial", y = "Response") +
      theme_minimal(base_size = 15) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold", size = 17),
            plot.subtitle = element_text(size = 13, color = "#666"),
            panel.grid.minor = element_blank())
  })

  output$alpha_s1a_display <- renderText({
    sprintf("%.2f", input$alpha_s1a)
  })

  # -- SOURCE 1B: DIFFERENT MECHANISMS, SAME BEHAVIOR --

  data_s1b <- reactive({
    req(input$simulate_s1b)

    n_trials <- input$n_trials_s1b
    n_shocks <- input$n_shocks_s1b

    shocks_A <- generate_shock_timing(input$timing_A_s1b, n_trials, n_shocks)
    shocks_B <- generate_shock_timing(input$timing_B_s1b, n_trials, n_shocks)

    dA <- simulate_rw_session(input$alpha_A_s1b, n_trials, shocks_A, sigma = 2.5)
    dA$person <- "A"
    dA$label <- sprintf("Person A (α = %.2f, %s sequence)", input$alpha_A_s1b, input$timing_A_s1b)
    dA$icon <- ifelse(dA$shock, "⚡", "☺")

    dB <- simulate_rw_session(input$alpha_B_s1b, n_trials, shocks_B, sigma = 2.5)
    dB$person <- "B"
    dB$label <- sprintf("Person B (α = %.2f, %s sequence)", input$alpha_B_s1b, input$timing_B_s1b)
    dB$icon <- ifelse(dB$shock, "⚡", "☺")

    list(
      data = rbind(dA, dB),
      stats_A = calculate_behavioral_indices(dA$response),
      stats_B = calculate_behavioral_indices(dB$response)
    )
  }) %>% bindEvent(input$simulate_s1b)

  output$plot_s1b <- renderPlot({
    req(data_s1b())
    d <- data_s1b()$data

    ggplot(d, aes(x = trial, y = response, color = label)) +
      geom_line(linewidth = 1.8) +
      geom_text(aes(label = icon), size = 7, family = "sans") +
      scale_color_manual(values = c("#4DBBD5", "#E64B35"), name = "") +
      labs(title = sprintf("Very Different Learning Rates (α = %.2f vs %.2f)",
                          input$alpha_A_s1b, input$alpha_B_s1b),
           subtitle = "⚡ = Shock trial   ☺ = Safe trial",
           x = "Trial", y = "Response") +
      theme_minimal(base_size = 15) +
      theme(legend.position = "top",
            plot.title = element_text(face = "bold", size = 17),
            plot.subtitle = element_text(size = 13, color = "#666"),
            panel.grid.minor = element_blank())
  })

  output$stats_s1b <- renderTable({
    req(data_s1b())
    d <- data_s1b()

    data.frame(
      Measure = c("Mean", "Slope", "First→Last", "Final"),
      `Person A` = c(
        sprintf("%.1f", d$stats_A$mean),
        sprintf("%.2f", d$stats_A$slope),
        sprintf("%.1f", d$stats_A$first_last_diff),
        sprintf("%.1f", d$stats_A$final_trial)
      ),
      `Person B` = c(
        sprintf("%.1f", d$stats_B$mean),
        sprintf("%.2f", d$stats_B$slope),
        sprintf("%.1f", d$stats_B$first_last_diff),
        sprintf("%.1f", d$stats_B$final_trial)
      ),
      Difference = c(
        sprintf("%.1f", abs(d$stats_A$mean - d$stats_B$mean)),
        sprintf("%.2f", abs(d$stats_A$slope - d$stats_B$slope)),
        sprintf("%.1f", abs(d$stats_A$first_last_diff - d$stats_B$first_last_diff)),
        sprintf("%.1f", abs(d$stats_A$final_trial - d$stats_B$final_trial))
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE)

  # -- SOURCE 2: MULTIPLE LATENT PROCESSES --

  data_s2 <- reactive({
    req(input$simulate_s2)

    n_trials <- input$n_trials_s2
    alpha_fast <- input$alpha_fast
    alpha_slow <- input$alpha_slow
    memory_fast <- input$memory_fast
    memory_slow <- input$memory_slow

    # Simulate Fast Learner
    sim_fast <- simulate_two_sessions_carryover(
      alpha = alpha_fast,
      memory_retention = memory_fast,
      n_trials_s1 = n_trials,
      n_trials_s2 = n_trials
    )
    sim_fast$learner <- "Fast Learner"
    sim_fast$alpha <- alpha_fast
    sim_fast$memory <- memory_fast

    # Simulate Slow Learner
    sim_slow <- simulate_two_sessions_carryover(
      alpha = alpha_slow,
      memory_retention = memory_slow,
      n_trials_s1 = n_trials,
      n_trials_s2 = n_trials
    )
    sim_slow$learner <- "Slow Learner"
    sim_slow$alpha <- alpha_slow
    sim_slow$memory <- memory_slow

    # Combine
    bind_rows(sim_fast, sim_slow)
  }) %>% bindEvent(input$simulate_s2)

  output$plot_s2_carryover <- renderPlot({
    req(data_s2())
    d <- data_s2()

    # Calculate mean starting points for subtitle
    mean_s1_start <- mean(d$response[d$session == 1 & d$trial == 1])
    mean_s2_start <- mean(d$response[d$session == 2 & d$trial == 1])

    # Create session labels and emoji icons
    d$session_label <- factor(
      paste("Session", d$session),
      levels = c("Session 1", "Session 2")
    )
    d$icon <- ifelse(d$shock, "⚡", "☺")

    # Get S2 Trial 1 values for equifinality annotation
    s2_t1_data <- d %>% filter(session == 2, trial == 1)
    s2_diff <- abs(diff(s2_t1_data$response))
    show_equifinality <- s2_diff < 10

    ggplot(d, aes(x = trial, y = response, color = learner, group = learner)) +
      geom_line(linewidth = 1.5, alpha = 0.8) +
      geom_point(size = 3, alpha = 0.7) +

      # Add emoji icons for shock/no-shock
      geom_text(aes(label = icon, y = response + 5),
                size = 6, family = "sans", show.legend = FALSE) +

      # Highlight Trial 1 with circles
      geom_point(
        data = d %>% filter(trial == 1),
        size = 6, shape = 21, stroke = 2, fill = NA
      ) +

      # Add equifinality annotation in Session 2 if applicable (just the line, no text)
      {if(show_equifinality) {
        annotate("segment",
                 x = 1, xend = 1,
                 y = min(s2_t1_data$response) - 3,
                 yend = max(s2_t1_data$response) + 3,
                 color = "#27AE60", linewidth = 1.5, linetype = "dashed")
      }} +

      # Facet by session
      facet_wrap(~ session_label, ncol = 2, scales = "free_x") +

      scale_color_manual(
        values = c("Fast Learner" = "#0072B2", "Slow Learner" = "#D55E00"),
        name = NULL
      ) +

      scale_x_continuous(breaks = seq(1, max(d$trial), by = 2)) +
      scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +

      labs(
        title = "Carry-over between sessions",
        subtitle = sprintf("Session 2 starts elevated (mean = %.0f) vs Session 1 naive (mean = %.0f)",
                          mean_s2_start, mean_s1_start),
        x = "Trial number (within session)",
        y = "US expectancy to CS+"
      ) +

      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0),
        plot.subtitle = element_text(color = "gray30", size = 13, margin = margin(b = 15), hjust = 0),
        strip.text = element_text(face = "bold", size = 15, color = "gray20"),
        strip.background = element_rect(fill = "gray95", color = NA),
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom",
        legend.text = element_text(size = 13),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1.5, "lines")
      )
  })

  output$stats_s2 <- renderUI({
    req(data_s2())
    d <- data_s2()

    # Get observed responses
    d_fast <- d %>% filter(learner == "Fast Learner")
    d_slow <- d %>% filter(learner == "Slow Learner")

    # Calculate behavioral indices for each learner
    # 1. Average response across both sessions
    avg_fast <- mean(d_fast$response)
    avg_slow <- mean(d_slow$response)

    # 2. First-to-last trial difference (learning magnitude)
    first_fast <- d_fast$response[1]
    last_fast <- d_fast$response[nrow(d_fast)]
    change_fast <- last_fast - first_fast

    first_slow <- d_slow$response[1]
    last_slow <- d_slow$response[nrow(d_slow)]
    change_slow <- last_slow - first_slow

    # Get parameters
    alpha_fast <- unique(d$alpha[d$learner == "Fast Learner"])[1]
    alpha_slow <- unique(d$alpha[d$learner == "Slow Learner"])[1]
    memory_fast <- unique(d$memory[d$learner == "Fast Learner"])[1]
    memory_slow <- unique(d$memory[d$learner == "Slow Learner"])[1]

    tagList(
      h5("Behavioral Marker:", style = "font-weight: bold; color: #1976D2;"),
      p(style = "font-size: 0.95em; margin: 5px 0; color: #0072B2;",
        strong("Fast"), sprintf(" (α=%.2f, m=%.1f)", alpha_fast, memory_fast)),
      p(style = "font-size: 0.85em; margin: 2px 0 2px 15px; color: #666;",
        sprintf("• Avg response: %.1f", avg_fast)),
      p(style = "font-size: 0.85em; margin: 2px 0 5px 15px; color: #666;",
        sprintf("• First→Last change: %.1f", change_fast)),

      p(style = "font-size: 0.95em; margin: 10px 0 5px 0; color: #D55E00;",
        strong("Slow"), sprintf(" (α=%.2f, m=%.1f)", alpha_slow, memory_slow)),
      p(style = "font-size: 0.85em; margin: 2px 0 2px 15px; color: #666;",
        sprintf("• Avg response: %.1f", avg_slow)),
      p(style = "font-size: 0.85em; margin: 2px 0 5px 15px; color: #666;",
        sprintf("• First→Last change: %.1f", change_slow))
    )
  })

  output$plot_s2_averaged <- renderPlot({
    req(data_s2())
    d <- data_s2()

    # Average across individuals (Fast + Slow) for each trial within each session
    d_avg <- d %>%
      group_by(trial, session) %>%
      summarise(avg_response = mean(response), .groups = 'drop')

    # Calculate overall average for each session
    session_avg <- d_avg %>%
      group_by(session) %>%
      summarise(mean_all = mean(avg_response), .groups = 'drop')

    avg_diff <- abs(diff(session_avg$mean_all))

    # Create session labels
    d_avg$session_label <- factor(
      paste("Session", d_avg$session),
      levels = c("Session 1", "Session 2")
    )

    ggplot(d_avg, aes(x = trial, y = avg_response, linetype = session_label, group = session_label)) +
      geom_line(linewidth = 2, color = "black", alpha = 0.85) +
      geom_point(size = 3.5, color = "black", alpha = 0.7) +

      scale_linetype_manual(
        values = c("Session 1" = "solid", "Session 2" = "dashed"),
        name = NULL
      ) +

      scale_x_continuous(breaks = seq(1, max(d_avg$trial), by = 2)) +
      scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +

      labs(
        title = "Session comparison",
        subtitle = sprintf("Session 1 avg = %.1f | Session 2 avg = %.1f | Difference = %.1f",
                          session_avg$mean_all[1], session_avg$mean_all[2], avg_diff),
        x = "Trial number (within session)",
        y = "US expectancy to CS+"
      ) +

      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 15, hjust = 0, color = "#2C3E50"),
        plot.subtitle = element_text(color = "gray30", size = 12, margin = margin(b = 10), hjust = 0),
        axis.title = element_text(face = "bold", size = 13),
        axis.text = element_text(size = 11),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        panel.grid.minor = element_blank()
      )
  })

  # -- SOURCE 3: MODEL MISSPECIFICATION --

  data_s3 <- reactive({
    req(input$simulate_s3)

    alpha <- input$alpha_s3
    kappa <- input$kappa_s3
    n_trials <- input$n_trials_s3
    reinforce_rate <- input$reinforce_rate_s3

    # Generate shock sequence (same for both)
    # Random shocks based on reinforcement rate
    n_shocks <- round(n_trials * reinforce_rate)
    shocks <- sort(sample(1:n_trials, n_shocks))

    # Simulate RW learner
    d_rw <- simulate_rw_session(alpha, n_trials, shocks, sigma = 2.5)
    d_rw$model <- "RW (Fixed α)"
    d_rw$learner <- sprintf("RW: α = %.2f (constant)", alpha)
    d_rw$icon <- ifelse(d_rw$shock, "⚡", "☺")
    d_rw$attention <- alpha  # Add constant attention for RW

    # Simulate PH learner
    d_ph <- simulate_ph_session(alpha, kappa, n_trials, shocks, sigma = 2.5)
    d_ph$model <- "Pearce-Hall (Dynamic α)"
    d_ph$learner <- sprintf("PH: α₀ = %.2f, κ = %.1f (dynamic)", alpha, kappa)
    d_ph$icon <- ifelse(d_ph$shock, "⚡", "☺")

    list(
      rw = d_rw,
      ph = d_ph,
      combined = rbind(d_rw, d_ph)
    )
  }) %>% bindEvent(input$simulate_s3)

  output$plot_s3_learning <- renderPlot({
    req(data_s3())
    d <- data_s3()$combined

    # Plot associative values (V) instead of responses to avoid ceiling effects
    ggplot(d, aes(x = trial, y = value, color = model)) +
      geom_line(linewidth = 1.8, alpha = 0.85) +
      geom_point(size = 2.5, alpha = 0.7) +
      geom_text(aes(label = icon, y = value + 0.08), size = 7, family = "sans", show.legend = FALSE) +
      scale_color_manual(
        values = c("RW (Fixed α)" = "#4DBBD5", "Pearce-Hall (Dynamic α)" = "#E64B35"),
        name = ""
      ) +
      scale_y_continuous(limits = c(-0.1, 1.1), breaks = seq(0, 1, 0.2)) +
      labs(
        title = sprintf("Same Initial α = %.2f, Same Reinforcement Rate (%.0f%%)",
                       input$alpha_s3, input$reinforce_rate_s3 * 100),
        subtitle = "⚡ = Shock trial   ☺ = Safe trial  |  Showing associative value (V), not response",
        x = "Trial",
        y = "Associative Value (V)\n0 = no learning, 1 = complete learning"
      ) +
      theme_minimal(base_size = 15) +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 17),
        plot.subtitle = element_text(size = 13, color = "#666"),
        panel.grid.minor = element_blank()
      )
  })

  output$plot_s3_attention <- renderPlot({
    req(data_s3())
    d_ph <- data_s3()$ph

    # Create data frame for attention
    d_att <- data.frame(
      trial = d_ph$trial,
      attention = d_ph$attention,
      icon = d_ph$icon
    )

    # Add constant RW line for comparison
    d_att$rw_alpha <- input$alpha_s3

    ggplot(d_att, aes(x = trial)) +
      geom_hline(aes(yintercept = rw_alpha, linetype = "RW (constant)"),
                color = "#4DBBD5", linewidth = 1.5, alpha = 0.7) +
      geom_line(aes(y = attention, linetype = "PH (dynamic)"),
               color = "#E64B35", linewidth = 1.8) +
      geom_point(aes(y = attention), color = "#E64B35", size = 2.5, alpha = 0.7) +
      geom_text(aes(y = attention + 0.05, label = icon),
               size = 6, family = "sans") +
      scale_linetype_manual(
        values = c("RW (constant)" = "dashed", "PH (dynamic)" = "solid"),
        name = "Learning Rate"
      ) +
      scale_y_continuous(limits = c(0, max(d_att$attention, input$alpha_s3) + 0.1)) +
      labs(
        title = "Learning Rate Over Time",
        x = "Trial",
        y = "Learning Rate (α / Attention)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 15),
        panel.grid.minor = element_blank()
      )
  })

  output$alpha_s3_display <- renderText({
    sprintf("%.2f", input$alpha_s3)
  })

}

shinyApp(ui = ui, server = server)
