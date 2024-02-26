tst1 <- readRDS("D:\\Malaria\\ResistancePaper\\outputs\\raw\\sitefile\\debug\\observed\\MLI\\site_data_interventions_MLI.RDS")
tst2 <- readRDS("D:\\Malaria\\ResistancePaper\\outputs\\raw\\sitefile\\debug\\PyOnly\\MLI\\site_data_interventions_MLI.RDS")

tst1$dn0 - tst2$dn0