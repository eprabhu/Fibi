package com.polus.fibicomp.award.vo;

import java.util.List;

import com.polus.fibicomp.award.pojo.AwardBasisOfPayment;
import com.polus.fibicomp.award.pojo.AwardMethodOfPayment;
import com.polus.fibicomp.award.pojo.Frequency;

public class AwardPaymentAndInvoicesVO {

	private String basisOfPaymentCode;

	private Integer awardId;

	private String methodOfPaymentCode;

	private String paymentInvoiceFrequencyCode;

	private Integer invoiceNoOfCopies;

	private Integer finalInvoiceDue;

	private String dfafsNumber;

	private String invoiceInstructions;

	private AwardMethodOfPayment awardMethodOfPayment;

	private AwardBasisOfPayment awardBasisOfPayment;
	
	private Frequency frequency;

	private List<AwardMethodOfPayment> awardMethodOfPayments;

	private List<AwardBasisOfPayment> awardBasisOfPayments;

	private List<Frequency> frequencies;

	private String updateUser;

	public String getBasisOfPaymentCode() {
		return basisOfPaymentCode;
	}

	public void setBasisOfPaymentCode(String basisOfPaymentCode) {
		this.basisOfPaymentCode = basisOfPaymentCode;
	}

	public String getMethodOfPaymentCode() {
		return methodOfPaymentCode;
	}

	public void setMethodOfPaymentCode(String methodOfPaymentCode) {
		this.methodOfPaymentCode = methodOfPaymentCode;
	}

	public Integer getInvoiceNoOfCopies() {
		return invoiceNoOfCopies;
	}

	public void setInvoiceNoOfCopies(Integer invoiceNoOfCopies) {
		this.invoiceNoOfCopies = invoiceNoOfCopies;
	}

	public Integer getFinalInvoiceDue() {
		return finalInvoiceDue;
	}

	public void setFinalInvoiceDue(Integer finalInvoiceDue) {
		this.finalInvoiceDue = finalInvoiceDue;
	}

	public String getDfafsNumber() {
		return dfafsNumber;
	}

	public void setDfafsNumber(String dfafsNumber) {
		this.dfafsNumber = dfafsNumber;
	}

	public String getInvoiceInstructions() {
		return invoiceInstructions;
	}

	public void setInvoiceInstructions(String invoiceInstructions) {
		this.invoiceInstructions = invoiceInstructions;
	}

	public AwardMethodOfPayment getAwardMethodOfPayment() {
		return awardMethodOfPayment;
	}

	public void setAwardMethodOfPayment(AwardMethodOfPayment awardMethodOfPayment) {
		this.awardMethodOfPayment = awardMethodOfPayment;
	}

	public AwardBasisOfPayment getAwardBasisOfPayment() {
		return awardBasisOfPayment;
	}

	public void setAwardBasisOfPayment(AwardBasisOfPayment awardBasisOfPayment) {
		this.awardBasisOfPayment = awardBasisOfPayment;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public void setPaymentInvoiceFrequencyCode(String paymentInvoiceFrequencyCode) {
		this.paymentInvoiceFrequencyCode = paymentInvoiceFrequencyCode;
	}

	public String getPaymentInvoiceFrequencyCode() {
		return paymentInvoiceFrequencyCode;
	}

	public List<AwardMethodOfPayment> getAwardMethodOfPayments() {
		return awardMethodOfPayments;
	}

	public void setAwardMethodOfPayments(List<AwardMethodOfPayment> awardMethodOfPayments) {
		this.awardMethodOfPayments = awardMethodOfPayments;
	}

	public List<AwardBasisOfPayment> getAwardBasisOfPayments() {
		return awardBasisOfPayments;
	}

	public void setAwardBasisOfPayments(List<AwardBasisOfPayment> awardBasisOfPayments) {
		this.awardBasisOfPayments = awardBasisOfPayments;
	}

	public Frequency getFrequency() {
		return frequency;
	}

	public void setFrequency(Frequency frequency) {
		this.frequency = frequency;
	}

	public List<Frequency> getFrequencies() {
		return frequencies;
	}

	public void setFrequencies(List<Frequency> frequencies) {
		this.frequencies = frequencies;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

}
