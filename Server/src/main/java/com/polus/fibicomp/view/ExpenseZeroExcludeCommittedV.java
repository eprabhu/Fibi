package com.polus.fibicomp.view;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.Immutable;

@Entity
@Immutable
@Table(name = "EXPENSE_ZERO_EXCLUDE_COMMIT_V")
public class ExpenseZeroExcludeCommittedV implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "DOCUMENT_NUMBER")
	private String documentNumber;

	@Id
	@Column(name = "ITEM_NUMBER")
	private String itemNumber;
	
	@Id
	@Column(name = "REFERENCE_DOCUMENT_CATEGORY")
	private String referenceDocumentCategory;
	
	@Id
	@Column(name = "REFERENCE_ORG_UNIT")
	private String referenceOrgUnit;
	
	@Id
	@Column(name = "ACCT_ASSIGNMENT_NUMBER")
	private String acctAssignmentNumber;
	
	@Id
	@Column(name = "SCHEDULE_LINE_NUMBER")
	private String scheduleLineNumber;
	
	@Id
	@Column(name = "CONDITION_COUNTER")
	private String conditionCounter;
	
	@Id
	@Column(name = "REFERENCE_PROCEDURE")
	private String referenceProcedure;
	
	@Id
	@Column(name = "DOCUMENT_NUMBER_FM_LINE_ITEM")
	private String documentNumberFMLineItem;
	
	@Id
	@Column(name = "TRANSACTION_NUMBER")
	private String transactionNumber;

	public String getDocumentNumber() {
		return documentNumber;
	}

	public void setDocumentNumber(String documentNumber) {
		this.documentNumber = documentNumber;
	}

	public String getItemNumber() {
		return itemNumber;
	}

	public void setItemNumber(String itemNumber) {
		this.itemNumber = itemNumber;
	}

	public String getReferenceDocumentCategory() {
		return referenceDocumentCategory;
	}

	public void setReferenceDocumentCategory(String referenceDocumentCategory) {
		this.referenceDocumentCategory = referenceDocumentCategory;
	}

	public String getReferenceOrgUnit() {
		return referenceOrgUnit;
	}

	public void setReferenceOrgUnit(String referenceOrgUnit) {
		this.referenceOrgUnit = referenceOrgUnit;
	}

	public String getAcctAssignmentNumber() {
		return acctAssignmentNumber;
	}

	public void setAcctAssignmentNumber(String acctAssignmentNumber) {
		this.acctAssignmentNumber = acctAssignmentNumber;
	}

	public String getScheduleLineNumber() {
		return scheduleLineNumber;
	}

	public void setScheduleLineNumber(String scheduleLineNumber) {
		this.scheduleLineNumber = scheduleLineNumber;
	}

	public String getConditionCounter() {
		return conditionCounter;
	}

	public void setConditionCounter(String conditionCounter) {
		this.conditionCounter = conditionCounter;
	}

	public String getReferenceProcedure() {
		return referenceProcedure;
	}

	public void setReferenceProcedure(String referenceProcedure) {
		this.referenceProcedure = referenceProcedure;
	}

	public String getDocumentNumberFMLineItem() {
		return documentNumberFMLineItem;
	}

	public void setDocumentNumberFMLineItem(String documentNumberFMLineItem) {
		this.documentNumberFMLineItem = documentNumberFMLineItem;
	}

	public String getTransactionNumber() {
		return transactionNumber;
	}

	public void setTransactionNumber(String transactionNumber) {
		this.transactionNumber = transactionNumber;
	}
	
}
