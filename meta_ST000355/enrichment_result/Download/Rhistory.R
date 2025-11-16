# PID of current job: 2769343
mSet<-InitDataObjects("conc", "msetora", FALSE, 150)
cmpd.vec<-c("Pentosidine","Phenyllactic acid","Glycerolphosphate","Glutamate","Benzoic acid","Aspartate","Glycerophosphocholine","α-ketoglutarate","5-Oxoproline","Arachidonic acid","Hypotaurine","Mannitol","Asparagine","Cystine")
mSet<-Setup.MapData(mSet, cmpd.vec);
mSet<-CrossReferencing(mSet, "name");
mSet<-CreateMappingResultTable(mSet)
mSet<-PerformDetailMatch(mSet, "Glycerolphosphate");
mSet<-GetCandidateList(mSet);
mSet<-SetCandidate(mSet, "Glycerolphosphate", "D-Alanyl-alanyl-poly(glycerolphosphate)");
mSet<-SetMetabolomeFilter(mSet, F);
mSet<-SetCurrentMsetLib(mSet, "kegg_pathway", 2);
mSet<-CalculateHyperScore(mSet)
mSet<-PlotORA(mSet, "ora_0_", "net", "png", 150, width=NA)
mSet<-PlotEnrichDotPlot(mSet, "ora", "ora_dot_0_", "png", 150, width=NA)
mSet<-CalculateHyperScore(mSet)
mSet<-PlotORA(mSet, "ora_1_", "net", "png", 150, width=NA)
mSet<-PlotEnrichDotPlot(mSet, "ora", "ora_dot_1_", "png", 150, width=NA)
mSet<-SaveTransformedData(mSet)
