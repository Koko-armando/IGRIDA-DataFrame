# R script
# 
# author: koko armando
library(dplyr)
library(GGally)
library(ggplot2)
library(scales)

# ------------------------------
# Functions definition
# ------------------------------

buildJHipsterDataframe <- function(csvInput){
  df <- data.frame(csvInput) %>%
    filter(!is.na(JHipsterRegister))
  
  # Add protractor, cucumber and gatling features
  df$protractor <- grepl('protractor', df$testFrameworks)
  df$cucumber <- grepl('cucumber', df$testFrameworks)
  df$gatling <- grepl('gatling', df$testFrameworks)
  df$testFrameworks <- NULL
  
  # Use logical for boolean features
  df$Docker <- as.logical(df$Docker)
  df$enableSocialSignIn <- as.logical(df$enableSocialSignIn)
  df$useSass <- as.logical(df$useSass)
  df$enableTranslation <- as.logical(df$enableTranslation)

  # Use strings for log entries
  df$Log.Gen <- as.character(df$Log.Gen)
  df$Log.Build <- as.character(df$Log.Build)
  df$Log.Compile <- as.character(df$Log.Compile)

  # # Qualify bug for each entry
  mutate(df,Bug='NA')
   bugs <- c('BUG:new','BUG:mariadbWithDocker', 'BUG:UAAAuthenticationWithDocker', 
             'BUG:mariadbWithGradle', 'BUG:OAUTH2', 'BUG:socialLoginWithMongoDB', 
             'BUG:UAAAuthenticationWithEhcache', 'ISSUE:env')
   df$Bug <- NA
  # # BUG:mariadbWithGradle
   df[grep("Error parsing reference: \"jhipster - jhipster-mariadb\" is not a valid repository/tag|Error parsing reference: \"jhipster - jhipster-mariadb:mariadb - jhipster-registry\" is not a valid repository/tag",
           df$Log.Build),]$Bug <- bugs[2]
  # # BUG:UAAAuthenticationWithDocker
   df[grep("java.lang.IllegalStateException: No instances available for uaa",
               df$Log.Build),][grep(TRUE,
                                    df[grep("java.lang.IllegalStateException: No instances available for uaa",
                                            df$Log.Build),]$Docker),]$Bug <- bugs[3]
  # # BUG:mariadbWithDocker
   df[grep("java.lang.RuntimeException: Failed to get driver instance for jdbcUrl=jdbc:mariadb://mariadb:3306/jhipster|java.lang.RuntimeException: Failed to get driver instance for jdbcUrl=jdbc:mariadb://localhost:3306/jhipster",
           df$Log.Build),]$Bug <- bugs[4]
  # # BUG:OAUTH2
   df[grep("JdbcTokenStore",
               df$Log.Build),]$Bug <- bugs[5]
  # # BUG:socialLoginWithMongoDB
   df[grep("SocialUserConnection",
               df$Log.Compile),]$Bug <- bugs[6]
  # # BUG:UAAAuthenticationWithEhcache
   df[grep("java.lang.IllegalStateException: No instances available for uaa",
               df$Log.Build),][grep(FALSE,df[grep("java.lang.IllegalStateException: No instances available for uaa",
                                                    df$Log.Build),]$Docker),]$Bug <- bugs[7]
  # # KO resolved by hand
   df <- resolvedByHand(df, bugs)
  
  return(df)
}

createHistogramBuild_AuthenticationType_Features <-function(cheminFichier){
data <- tbl_df(read.csv(cheminFichier))
ggplot(data) + aes(x = authenticationType, fill = Build)+
geom_bar(position = "fill") + xlab("Authentication Type")+ylab("Proportion") + labs(fill = "Buid Result") + scale_y_continuous(labels = percent)
ggsave("AuthenticationType_Build_Result.jpg")

  }


createHistogramCompile_AuthenticationType_Features <-function(cheminFichier){
  data <- tbl_df(read.csv(cheminFichier))
ggplot(data) + aes(x = authenticationType, fill = Compile) + geom_bar(position = "fill") + xlab("Authentication Type") +  ylab("Proportion") + labs(fill = "Compile Result") + scale_y_continuous(labels = percent)
ggsave("AuthenticationType_Compile_Result.jpg")
  }



createHistogramBuild_ApplicationType_Features <-function(cheminFichier){
  data <- tbl_df(read.csv(cheminFichier))
ggplot(data) + aes(x = applicationType, fill = Build) + geom_bar(position = "fill") + xlab("Application Type") +  ylab("Proportion") + labs(fill = "Buid Result") + scale_y_continuous(labels = percent)
ggsave("ApplicationType_Build_Result.jpg")
  }

createHistogramCompile_ApplicationType_Features <-function(cheminFichier){
  data <- tbl_df(read.csv(cheminFichier))
ggplot(data) + aes(x = applicationType, fill = Compile) + geom_bar(position = "fill") + xlab("Application Type") +  ylab("Proportion") + labs(fill = "Compile Result") + scale_y_continuous(labels = percent)
ggsave("ApplicationType_Compile_Result.jpg")
  }	

createHistogrameofBuid <-function(cheminFichier){
data <- tbl_df(read.csv(cheminFichier))
ggplot(data) + aes(x = Build) + geom_bar(width = 0.2) + xlab("Build Result") +  ylab("Proportion")
ggsave("Build_Result.jpg")
}





create_Histograme_of_repartition_of_BuidKO_AuthenticationType <-function(cheminFichier){
data <- tbl_df(read.csv(cheminFichier))
Plot <-ggpairs(data[, c("authenticationType", "Build")], aes(colour = Build))
png("repartition_of_BuidKO_AuthenticationType.png")
print(Plot)
#ggsave("repartition_of_BuidKO_AuthenticationType.jpg")

}

create_Histograme_of_repartition_of_CompileKO_AuthenticationType <-function(cheminFichier){
data <- tbl_df(read.csv(cheminFichier))
Plot <-ggpairs(data[, c("authenticationType", "Compile")], aes(colour = Compile))
png("repartition_of_compileKO_AuthenticationType.png")
print(Plot)
#ggsave("repartition_of_compileKO_AuthenticationType.jpg")

}


create_Histograme_of_repartition_of_CompileKO_ApplicationType <-function(cheminFichier){
data <- tbl_df(read.csv(cheminFichier))
Plot <-ggpairs(data[, c("applicationType", "Compile")], aes(colour = Compile))
png("repartition_of_compileKO_ApplicationType.png")
print(Plot)
#ggsave("repartition_of_compileKO_ApplicationType.jpg")

}


create_Histograme_of_repartition_of_BuidKO_ApplicationType <-function(cheminFichier){
data <- tbl_df(read.csv(cheminFichier))
Plot <-ggpairs(data[, c("applicationType", "Build")], aes(colour = Build))
png("repartition_of_BuidKO_ApplicationType.png")
print(Plot)
#ggsave("repartition_of_BuidKO_ApplicationType.jpg")

}
# ------------------------------
# Main function definition
# ------------------------------

main <- function(args){
  # Process JHipster main file
  csvInput <- read.csv(file=args[1], na.strings = c("", "NA", "ND"), head=TRUE, sep=';', stringsAsFactors = FALSE)
  jhipster <- buildJHipsterDataframe(csvInput)
  saveRDS(jhipster, file = 'jhipster-results.Rda')
  write.csv(jhipster, file = 'jhipster-results.csv')

createHistogramBuild_AuthenticationType_Features('jhipster-results.csv')
createHistogramCompile_AuthenticationType_Features('jhipster-results.csv')
createHistogramBuild_ApplicationType_Features('jhipster-results.csv')
createHistogramCompile_ApplicationType_Features('jhipster-results.csv')

create_Histograme_of_repartition_of_BuidKO_AuthenticationType('jhipster-results.csv')

create_Histograme_of_repartition_of_CompileKO_AuthenticationType('jhipster-results.csv')

create_Histograme_of_repartition_of_CompileKO_ApplicationType('jhipster-results.csv')

create_Histograme_of_repartition_of_BuidKO_ApplicationType('jhipster-results.csv')

}

# ------------------------------
# Program
# ------------------------------

#args=c("tmp/jhipster.csv")

args <- commandArgs(trailingOnly = TRUE)
main(args)
