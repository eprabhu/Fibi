package com.polus.fibicomp.agreements.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.agreements.dto.PlaceHolderDTO;
import com.polus.fibicomp.agreements.pojo.AgreementCategory;
import com.polus.fibicomp.agreements.pojo.AgreementClausesGroupMapping;
import com.polus.fibicomp.agreements.pojo.AgreementPlaceHolder;
import com.polus.fibicomp.agreements.pojo.AgreementType;
import com.polus.fibicomp.agreements.pojo.AgreementTypeTemplate;
import com.polus.fibicomp.agreements.pojo.ClausesBank;
import com.polus.fibicomp.agreements.pojo.ClausesGroup;

public class TemplateManagementVO {

	private ClausesGroup clausesGroup;

	private AgreementClausesGroupMapping agreementClausesGroupMapping;

	private List<AgreementPlaceHolder> agreementPlaceHolder;

	private List<ClausesGroup> clausesDetails;

	private List<AgreementCategory> agreementCategories;

	private List<AgreementTypeTemplate> agreementTypeTemplates;

	private List<AgreementType> agreementTypes;

	private List<PlaceHolderDTO> placeHolders;

	private String agreementTypeCode;

	private String clausesGroupCode;

	private Integer clausesCode;

	private Integer templateId;

	private String searchString;

	private ClausesBank clausesBank;

	private List<ClausesBank> clausesBanks;

	private Integer clauseCode;

	public TemplateManagementVO() {
		clausesBanks = new ArrayList<>();
	}

	public ClausesGroup getClausesGroup() {
		return clausesGroup;
	}

	public void setClausesGroup(ClausesGroup clausesGroup) {
		this.clausesGroup = clausesGroup;
	}

	public AgreementClausesGroupMapping getAgreementClausesGroupMapping() {
		return agreementClausesGroupMapping;
	}

	public void setAgreementClausesGroupMapping(AgreementClausesGroupMapping agreementClausesGroupMapping) {
		this.agreementClausesGroupMapping = agreementClausesGroupMapping;
	}

	public List<AgreementPlaceHolder> getAgreementPlaceHolder() {
		return agreementPlaceHolder;
	}

	public void setAgreementPlaceHolder(List<AgreementPlaceHolder> agreementPlaceHolder) {
		this.agreementPlaceHolder = agreementPlaceHolder;
	}

	public List<AgreementCategory> getAgreementCategories() {
		return agreementCategories;
	}

	public void setAgreementCategories(List<AgreementCategory> agreementCategories) {
		this.agreementCategories = agreementCategories;
	}

	public List<AgreementType> getAgreementTypes() {
		return agreementTypes;
	}

	public void setAgreementTypes(List<AgreementType> agreementTypes) {
		this.agreementTypes = agreementTypes;
	}

	public List<ClausesGroup> getClausesDetails() {
		return clausesDetails;
	}

	public void setClausesDetails(List<ClausesGroup> clausesDetails) {
		this.clausesDetails = clausesDetails;
	}

	public String getAgreementTypeCode() {
		return agreementTypeCode;
	}

	public void setAgreementTypeCode(String agreementTypeCode) {
		this.agreementTypeCode = agreementTypeCode;
	}

	public List<AgreementTypeTemplate> getAgreementTypeTemplates() {
		return agreementTypeTemplates;
	}

	public void setAgreementTypeTemplates(List<AgreementTypeTemplate> agreementTypeTemplates) {
		this.agreementTypeTemplates = agreementTypeTemplates;
	}

	public List<PlaceHolderDTO> getPlaceHolders() {
		return placeHolders;
	}

	public void setPlaceHolders(List<PlaceHolderDTO> placeHolders) {
		this.placeHolders = placeHolders;
	}

	public Integer getClausesCode() {
		return clausesCode;
	}

	public void setClausesCode(Integer clausesCode) {
		this.clausesCode = clausesCode;
	}

	public String getClausesGroupCode() {
		return clausesGroupCode;
	}

	public void setClausesGroupCode(String clausesGroupCode) {
		this.clausesGroupCode = clausesGroupCode;
	}

	public Integer getTemplateId() {
		return templateId;
	}

	public void setTemplateId(Integer templateId) {
		this.templateId = templateId;
	}

	public String getSearchString() {
		return searchString;
	}

	public void setSearchString(String searchString) {
		this.searchString = searchString;
	}

	public ClausesBank getClausesBank() {
		return clausesBank;
	}

	public void setClausesBank(ClausesBank clausesBank) {
		this.clausesBank = clausesBank;
	}

	public List<ClausesBank> getClausesBanks() {
		return clausesBanks;
	}

	public void setClausesBanks(List<ClausesBank> clausesBanks) {
		this.clausesBanks = clausesBanks;
	}

	public Integer getClauseCode() {
		return clauseCode;
	}

	public void setClauseCode(Integer clauseCode) {
		this.clauseCode = clauseCode;
	}
}
