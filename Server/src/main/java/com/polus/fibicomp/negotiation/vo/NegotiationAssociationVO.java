package com.polus.fibicomp.negotiation.vo;

import com.polus.fibicomp.negotiation.dto.NegotiationProjectDetailsDto;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociation;
import com.polus.fibicomp.negotiation.pojo.NegotiationsAssociationDetails;

public class NegotiationAssociationVO {

	private NegotiationsAssociation negotiationsAssociations;
	private NegotiationsAssociationDetails negotiationsAssociationDetails;
	private NegotiationProjectDetailsDto projectDetails;
	String acType;
	String associationTypeCode;
	
	public String getAcType() {
		return acType;
	}
	
	public void setAcType(String acType) {
		this.acType = acType;
	}

	public String getAssociationTypeCode() {
		return associationTypeCode;
	}

	public void setAssociationTypeCode(String associationTypeCode) {
		this.associationTypeCode = associationTypeCode;
	}
	
	public NegotiationsAssociationDetails getNegotiationsAssociationDetails() {
		return negotiationsAssociationDetails;
	}

	public void setNegotiationsAssociationDetails(NegotiationsAssociationDetails negotiationsAssociationDetails) {
		this.negotiationsAssociationDetails = negotiationsAssociationDetails;
	}

	public NegotiationProjectDetailsDto getProjectDetails() {
		return projectDetails;
	}

	public void setProjectDetails(NegotiationProjectDetailsDto projectDetails) {
		this.projectDetails = projectDetails;
	}

	public NegotiationsAssociation getNegotiationsAssociations() {
		return negotiationsAssociations;
	}

	public void setNegotiationsAssociations(NegotiationsAssociation negotiationsAssociations) {
		this.negotiationsAssociations = negotiationsAssociations;
	}
	
}
