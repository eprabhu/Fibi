package com.polus.fibicomp.negotiation.dto;

public class NegotiationReportDto {

	String negotiatorName;
	String start_date;
	String end_date;
	String StatusCode;
	String Agreement_type;
	String Summary_comment;
	String Legal_comment;
	String Neotiator_comment;
	String Location;
	String CreatedBy;
	
	public String getNegotiatorName() {
		return negotiatorName;
	}
	public void setNegotiatorName(String negotiatorName) {
		this.negotiatorName = negotiatorName;
	}
	public String getStart_date() {
		return start_date;
	}
	public void setStart_date(String start_date) {
		this.start_date = start_date;
	}
	public String getEnd_date() {
		return end_date;
	}
	public void setEnd_date(String end_date) {
		this.end_date = end_date;
	}
	public String getStatusCode() {
		return StatusCode;
	}
	public void setStatusCode(String statusCode) {
		StatusCode = statusCode;
	}
	public String getAgreement_type() {
		return Agreement_type;
	}
	public void setAgreement_type(String agreement_type) {
		Agreement_type = agreement_type;
	}
	public String getSummary_comment() {
		return Summary_comment;
	}
	public void setSummary_comment(String summary_comment) {
		Summary_comment = summary_comment;
	}
	public String getLegal_comment() {
		return Legal_comment;
	}
	public void setLegal_comment(String legal_comment) {
		Legal_comment = legal_comment;
	}
	public String getNeotiator_comment() {
		return Neotiator_comment;
	}
	public void setNeotiator_comment(String neotiator_comment) {
		Neotiator_comment = neotiator_comment;
	}
	public String getLocation() {
		return Location;
	}
	public void setLocation(String location) {
		Location = location;
	}
	public String getCreatedBy() {
		return CreatedBy;
	}
	public void setCreatedBy(String createdBy) {
		CreatedBy = createdBy;
	}
	
}
