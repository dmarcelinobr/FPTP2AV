#' @encoding UTF-8
#' @title Simulate election outcomes for AV given FPTP votes.
#'
#' @description This program simulates election outcomes for  the Alternative Vote System using the actual First-Past-The-Post votes. It takes a wide table with the election results and returns a \code{csv} file with simulations for further analysis.
#'
#' @param data A flat table or data frame containing FPTP votes for each district.
#'
#' @param link A link function containing a structure for candidates/parties relations.
#'
#' @return A text file \code{(csv)} containing simulated electoral results for each district.
#' @references
#' Marcelino, Daniel (2013). \emph{SciencesPo: A Tool Set for Analyzing Political Behaviour Data}. Available at SSRN: \url{http://dx.doi.org/10.2139/ssrn.2320547}
#'
#' @note Unfortunately, this function is not so flexible regarding the data supplied. For it to work properly, the input table should have  the same format (flat table) as the example data provided within this package. Namely, the same structure with an Id column, the name of the constituencies in another column, and each party achievements distributed in columns.  Besides the dataset, a "link" structure for party/candidates' relations is also necessary.
#'
#' @export
#'
#' @examples
#' #Feeding data:
#' data(ge2010)
#'
#' # Let's take a small portion of the data.
#' IR <-ge2010[ge2010$Region=="Northern Ireland",]
#'
#' # First, we create vectors for parties/candidates link
#' # For simplicity, I'm considering only parties which received some votes.
#'
#' APNI<- c("SF", "DUP", "SDLP", "TUV", "UCUNF")
#' BNP<-c("Ch P", "Con", "CPA", "NF", "UKIP");
#' Ch_P<-c("BNP", "CPA", "Con", "ED")
#' Con<-c("LD", "UKIP", "ED", "Ch P", "CPA")
#' CPA<-c("BNP", "Ch P", "Con", "ED")
#' DUP<-c("SDLP", "TUV", "UCUNF", "APNI")
#' ED<-c("Con", "BNP", "NF", "CPA", "Ch P")
#' Grn<-c("LD", "Lab", "SDLP", "PC", "SSP", "TUSC", "TUV")
#' Lab<-c("LD", "Grn", "SDLP", "Soc", "SSP", "TUSC", "TUV")
#' LD<-c("Lab", "Con", "Grn", "PC", "SDLP")
#' MRLP<-c("Con", "Lab", "LD", "Grn", "PC", "SDLP", "SNP", "SSP", "UKIP")
#' NF<-c("BNP", "Con", "ED", "UKIP")
#' PC<-c("Con", "Lab", "LD")
#' Respect<-c("Con", "Lab", "LD")
#' SDLP<-c("Grn", "Lab", "LD")
#' SF<-c("APNI", "USUNF")
#' Soc<-c("Lab", "SSP", "SDLP", "TUSC", "TUV")
#' SNP<-c("Ch P", "CPA", "UKIP")
#' SSP<-c("Grn", "Lab", "LD", "SDLP", "TUSC", "TUV")
#' TUSC<-c("SDLP", "Soc", "SSP", "TUV")
#' TUV<-c("SDLP", "Soc", "SSP", "TUSC")
#' UCUNF<-c("APNI", "SF", "DUP")
#' UKIP<-c("BNP", "NF", "Con")
#'
#' ## vector with party/candidate relations:
#'
#' party_chains <-structure(list(APNI,BNP,Ch_P,Con,CPA,DUP,ED,
#' Grn, Lab,LD,MRLP,NF,PC,Respect,SDLP,SF,Soc,SNP,SSP,TUSC,TUV,UCUNF,
#'  UKIP), .Names = c("APNI","BNP","Ch_P","Con","CPA","DUP","ED","Grn",
#'  "Lab","LD","MRLP","NF","PC","Respect","SDLP","SF","Soc","SNP","SSP",
#'  "TUSC", "TUV","UCUNF","UKIP" ));
#'
#' ## Run the algorithm
#' \dontrun{fptp2av(data=IR, link=party_chains)}
#'
#' @keywords Simulations
#' @importFrom data.table data.table :=
#' @importFrom utils write.csv
#' @importFrom stats na.omit complete.cases
fptp2av <- function(data=NULL, link=NULL){
  candidates <- NULL
  vote_spread <- NULL
	if(is.null(data)){
	stop("A valid FPTP vote dataset is needed to perform the simulations.")
	}
	else{
		if(is.null(link)){
		stop("A valid link relation for candidates/parties is expected.")
		}
		else{
	start_time<-(Sys.time())
column.names<-c("District", "Turnout", "Outcome", "FPTP.Winner", "FPTP.Winner.Votes", "AV.Winner", "AV.Winner.Votes")
data.to.read<-data.table::data.table(data)
parties<-names(data.to.read)[c(7:ncol(data.to.read))]

first<-T
for(row in 1:(nrow(data.to.read)-1)){
#for (row in 1:4){
  each_row<-cbind(c(names(data.to.read[row])),c(t(data.to.read[row])))
  name<-each_row[2,2]
  electorate<-as.integer(each_row[6,2])
  each_row<-data.table::as.data.table(each_row[complete.cases(each_row),])
  each_row<-each_row[7:nrow(each_row)]
  data.table::setnames(each_row,c("V1","V2"),c("candidates", "vote_spread"))

	print(paste(name," - ",electorate," - ", paste(each_row[,candidates], collapse=" ")) )
	print(each_row)
	#write the data to a matrix
	if(first==T){
	first=F
	new.colnames<-c("District", "Turnout",
		 "Outcome", "FPTP.Winner", "FPTP.Winner.Votes", "AV.Winner", "AV.Winner.Votes", "Rounds")
	data.to.write<-matrix(nrow=0,ncol=8,dimnames=list(c(),new.colnames))
	}
	votess<-.District(name, electorate, each_row, link, 4)
	counter<-0
	results_vec<-.get_results(votess, counter)
  #print(.District(name, electorate, each_row, link,4))
	fptp_winner<-results_vec[[1]]
	fptp_votes<-results_vec[[2]]
	av_winner<-results_vec[[3]]
	av_votes<- results_vec[[4]]
	av_counter<- results_vec[[5]]
if(fptp_winner==av_winner){outcome<-"Unaltered"} 	else{outcome<-"Altered"}
	newrow<-c(name,toString(electorate),outcome,fptp_winner,toString(fptp_votes),av_winner,toString(av_votes),toString(av_counter))
	data.to.write<-rbind(data.to.write,c(newrow))
	message("Done at ", .timediff(start_time))
	}
outfile <- paste("Sim_data_in",format(Sys.time(),".%d.%b.%Y-%H:%M:%S"),".csv",sep="")
write.csv(data.to.write, file=outfile, row.names=FALSE)
		}
	}
}
NULL
