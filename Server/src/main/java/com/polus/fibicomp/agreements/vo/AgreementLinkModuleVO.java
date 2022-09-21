package com.polus.fibicomp.agreements.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.agreements.pojo.AgreementAssociationDetail;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.pojo.AgreementPeople;
import com.polus.fibicomp.agreements.pojo.AgreementSponsor;

public class AgreementLinkModuleVO {

	private Integer agreementRequestId;

	private AgreementAssociationDetail agreementAssociationDetail;

	private AgreementHeader agreementHeader;

	private String updateUser;

	private String acType;

	private String personId;

	private List<String> availableRights;

	private Integer moduleCode;

	private String moduleItemKey;

	private List<AgreementAssociationDetail> agreementAssociationDetails;

	private List<AgreementPeople> agreementPeoples;

	private List<AgreementSponsor> agreementSponsors;

	public AgreementLinkModuleVO() {
		agreementSponsors = new ArrayList<>();
		agreementPeoples = new ArrayList<>();
	}

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public AgreementHeader getAgreementHeader() {
		return agreementHeader;
	}

	public void setAgreementHeader(AgreementHeader agreementHeader) {
		this.agreementHeader = agreementHeader;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public List<String> getAvailableRights() {
		return availableRights;
	}

	public void setAvailableRights(List<String> availableRights) {
		this.availableRights = availableRights;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public AgreementAssociationDetail getAgreementAssociationDetail() {
		return agreementAssociationDetail;
	}

	public void setAgreementAssociationDetail(AgreementAssociationDetail agreementAssociationDetail) {
		this.agreementAssociationDetail = agreementAssociationDetail;
	}

	public List<AgreementAssociationDetail> getAgreementAssociationDetails() {
		return agreementAssociationDetails;
	}

	public void setAgreementAssociationDetails(List<AgreementAssociationDetail> agreementAssociationDetails) {
		this.agreementAssociationDetails = agreementAssociationDetails;
	}

	public List<AgreementPeople> getAgreementPeoples() {
		return agreementPeoples;
	}

	public void setAgreementPeoples(List<AgreementPeople> agreementPeoples) {
		this.agreementPeoples = agreementPeoples;
	}

	public List<AgreementSponsor> getAgreementSponsors() {
		return agreementSponsors;
	}

	public void setAgreementSponsors(List<AgreementSponsor> agreementSponsors) {
		this.agreementSponsors = agreementSponsors;
	}

}
