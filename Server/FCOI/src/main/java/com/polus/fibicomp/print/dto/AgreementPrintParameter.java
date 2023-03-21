package com.polus.fibicomp.print.dto;

import java.util.ArrayList;
import java.util.List;

public class AgreementPrintParameter {

	private String personType;

	private String personName;

	private String personEmail;

	private String personPhone;

	private String personDepartment;

	private String clausesGroupName;

	private String clauses;

	private String sponsorName;

	private String sponsorType;

	private String sponsorAddress;

	private String sponsorLocation;

	private String sponsorState;

	private String sponsorCountry;

	private String sponsorZip;

	private String sponsorRole;

	private String sponsorAgreementType;

	private List<AgreementSponsorContactParameter> agreementSponsorContacts;

	public AgreementPrintParameter() {
		agreementSponsorContacts = new ArrayList<>();
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public void setSponsorName(String sponsorName) {
		if (sponsorName != null) {
			this.sponsorName = sponsorName;
		} else {
			this.sponsorName = "";
		}
	}

	public String getSponsorType() {
		return sponsorType;
	}

	public void setSponsorType(String sponsorType) {
		if (sponsorType != null) {
			this.sponsorType = sponsorType;
		} else {
			this.sponsorType = "";
		}
	}

	public String getSponsorAddress() {
		return sponsorAddress;
	}

	public void setSponsorAddress(String sponsorAddress) {
		if (sponsorAddress != null) {
			this.sponsorAddress = sponsorAddress;
		} else {
			this.sponsorAddress = "";
		}
	}

	public String getSponsorLocation() {
		return sponsorLocation;
	}

	public void setSponsorLocation(String sponsorLocation) {
		if (sponsorLocation != null) {
			this.sponsorLocation = sponsorLocation;
		} else {
			this.sponsorLocation = "";
		}
	}

	public String getSponsorState() {
		return sponsorState;
	}

	public void setSponsorState(String sponsorState) {
		if (sponsorState != null) {
			this.sponsorState = sponsorState;
		} else {
			this.sponsorState = "";
		}
	}

	public String getSponsorCountry() {
		return sponsorCountry;
	}

	public void setSponsorCountry(String sponsorCountry) {
		if (sponsorCountry != null) {
			this.sponsorCountry = sponsorCountry;
		} else {
			this.sponsorCountry = "";
		}
	}

	public String getSponsorZip() {
		return sponsorZip;
	}

	public void setSponsorZip(String sponsorZip) {
		if (sponsorZip != null) {
			this.sponsorZip = sponsorZip;
		} else {
			this.sponsorZip = "";
		}
	}


	public String getPersonType() {
		return personType;
	}

	public void setPersonType(String personType) {
		if (personType != null) {
			this.personType = personType;
		} else {
			this.personType = "";
		}
	}

	public String getPersonName() {
		return personName;
	}

	public void setPersonName(String personName) {
		if (personName != null) {
			this.personName = personName;
		} else {
			this.personName = "";
		}
	}

	public String getPersonEmail() {
		return personEmail;
	}

	public void setPersonEmail(String personEmail) {
		if (personEmail != null) {
			this.personEmail = personEmail;
		} else {
			this.personEmail = "";
		}
	}

	public String getPersonPhone() {
		return personPhone;
	}

	public void setPersonPhone(String personPhone) {
		if (personPhone != null) {
			this.personPhone = personPhone;
		} else {
			this.personPhone = "";
		}
	}

	public String getPersonDepartment() {
		return personDepartment;
	}

	public void setPersonDepartment(String personDepartment) {
		if (personDepartment != null) {
			this.personDepartment = personDepartment;
		} else {
			this.personDepartment = "";
		}
	}

	public String getClausesGroupName() {
		return clausesGroupName;
	}

	public void setClausesGroupName(String clausesGroupName) {
		if (clausesGroupName != null) {
			this.clausesGroupName = clausesGroupName;
		} else {
			this.clausesGroupName = "";
		}
	}

	public String getClauses() {
		return clauses;
	}

	public void setClauses(String clauses) {
		this.clauses = clauses;
	}

	public List<AgreementSponsorContactParameter> getAgreementSponsorContacts() {
		return agreementSponsorContacts;
	}

	public void setAgreementSponsorContacts(List<AgreementSponsorContactParameter> agreementSponsorContacts) {
		this.agreementSponsorContacts = agreementSponsorContacts;
	}

	public String getSponsorRole() {
		return sponsorRole;
	}

	public void setSponsorRole(String sponsorRole) {
		if (sponsorRole != null) {
			this.sponsorRole = sponsorRole;
		} else {
			this.sponsorRole = "";
		}
	}

	public String getSponsorAgreementType() {
		return sponsorAgreementType;
	}

	public void setSponsorAgreementType(String sponsorAgreementType) {
		if (sponsorAgreementType != null) {
			this.sponsorAgreementType = sponsorAgreementType;
		} else {
			this.sponsorAgreementType = "";
		}
	}

}
