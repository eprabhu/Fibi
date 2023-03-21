package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.pojo.Currency;

@Entity
@Table(name = "AWARD_AMOUNT_INFO")
public class AwardAmountInfo implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_AMOUNT_INFO_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_AMOUNT_INFO_ID_GENERATOR")
	@SequenceGenerator(name="AWARD_AMOUNT_INFO_ID_GENERATOR", sequenceName = "AWARD_AMOUNT_INFO_ID_GENERATOR", allocationSize=1)
	private Integer awardAmountInfoId;

	@Column(name = "AWARD_ID") 
	private Integer awardId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_AMOUNT_INFO_FK1"), name = "AWARD_ID", referencedColumnName = "AWARD_ID", insertable = false, updatable = false)
	private Award award;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "TRANSACTION_ID")
	private BigDecimal transactionId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_AMOUNT_INFO_FK2"), name = "TRANSACTION_ID", referencedColumnName = "TRANSACTION_ID", insertable = false, updatable = false)
	private AwardAmountTransaction awardAmountTransaction;

	@Column(name = "ANTICIPATED_CHANGE")
	private BigDecimal anticipatedChange = BigDecimal.ZERO;

	@Column(name = "ANTICIPATED_CHANGE_DIRECT")
	private BigDecimal anticipatedChangeDirect = BigDecimal.ZERO;

	@Column(name = "ANTICIPATED_CHANGE_INDIRECT")
	private BigDecimal anticipatedChangeIndirect = BigDecimal.ZERO;

	@Column(name = "OBLIGATED_CHANGE")
	private BigDecimal obligatedChange = BigDecimal.ZERO;

	@Column(name = "OBLIGATED_CHANGE_DIRECT")
	private BigDecimal obligatedChangeDirect = BigDecimal.ZERO;

	@Column(name = "OBLIGATED_CHANGE_INDIRECT")
	private BigDecimal obligatedChangeIndirect = BigDecimal.ZERO;

	@Column(name = "ANTICIPATED_TOTAL_DIRECT")
	private BigDecimal anticipatedTotalDirect = BigDecimal.ZERO;

	@Column(name = "ANTICIPATED_TOTAL_INDIRECT")
	private BigDecimal anticipatedTotalIndirect = BigDecimal.ZERO;

	@Column(name = "ANTICIPATED_TOTAL_AMOUNT")
	private BigDecimal anticipatedTotalAmount = BigDecimal.ZERO;

	@Column(name = "OBLIGATED_TOTAL_DIRECT")
	private BigDecimal obligatedTotalDirect = BigDecimal.ZERO;

	@Column(name = "OBLIGATED_TOTAL_INDIRECT")
	private BigDecimal obligatedTotalIndirect = BigDecimal.ZERO;

	@Column(name = "AMOUNT_OBLIGATED_TO_DATE")
	private BigDecimal amountObligatedToDate = BigDecimal.ZERO;

	@Column(name = "ANT_DISTRIBUTABLE_AMOUNT")
	private BigDecimal antDistributableAmount = BigDecimal.ZERO;

	@Column(name = "OBLI_DISTRIBUTABLE_AMOUNT")
	private BigDecimal obliDistributableAmount = BigDecimal.ZERO;

	@Column(name = "FINAL_EXPIRATION_DATE")
	private Timestamp finalExpirationDate;

	@Column(name = "CURRENT_FUND_EFFECTIVE_DATE")
	private Timestamp currentFundEffectiveDate;

	@Column(name = "OBLIGATION_EXPIRATION_DATE")
	private Timestamp obligationExpirationDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "TOTAL_COST_IN_CURRENCY")
	private BigDecimal totalCostInCurrency = BigDecimal.ZERO;

	@Column(name = "CURRENCY_CODE")
	private String currencyCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_AMOUNT_INFO_FK3"), name = "CURRENCY_CODE", referencedColumnName = "CURRENCY_CODE", insertable = false, updatable = false)
	private Currency currency;

	@Transient
	private String updateUserFullName;

	public Integer getAwardAmountInfoId() {
		return awardAmountInfoId;
	}

	public void setAwardAmountInfoId(Integer awardAmountInfoId) {
		this.awardAmountInfoId = awardAmountInfoId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public Award getAward() {
		return award;
	}

	public void setAward(Award award) {
		this.award = award;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public BigDecimal getTransactionId() {
		return transactionId;
	}

	public void setTransactionId(BigDecimal transactionId) {
		this.transactionId = transactionId;
	}

	public AwardAmountTransaction getAwardAmountTransaction() {
		return awardAmountTransaction;
	}

	public void setAwardAmountTransaction(AwardAmountTransaction awardAmountTransaction) {
		this.awardAmountTransaction = awardAmountTransaction;
	}

	public BigDecimal getAnticipatedChange() {
		return anticipatedChange;
	}

	public void setAnticipatedChange(BigDecimal anticipatedChange) {
		this.anticipatedChange = anticipatedChange;
	}

	public BigDecimal getAnticipatedChangeDirect() {
		return anticipatedChangeDirect;
	}

	public void setAnticipatedChangeDirect(BigDecimal anticipatedChangeDirect) {
		this.anticipatedChangeDirect = anticipatedChangeDirect;
	}

	public BigDecimal getAnticipatedChangeIndirect() {
		return anticipatedChangeIndirect;
	}

	public void setAnticipatedChangeIndirect(BigDecimal anticipatedChangeIndirect) {
		this.anticipatedChangeIndirect = anticipatedChangeIndirect;
	}

	public BigDecimal getObligatedChange() {
		return obligatedChange;
	}

	public void setObligatedChange(BigDecimal obligatedChange) {
		this.obligatedChange = obligatedChange;
	}

	public BigDecimal getObligatedChangeDirect() {
		return obligatedChangeDirect;
	}

	public void setObligatedChangeDirect(BigDecimal obligatedChangeDirect) {
		this.obligatedChangeDirect = obligatedChangeDirect;
	}

	public BigDecimal getObligatedChangeIndirect() {
		return obligatedChangeIndirect;
	}

	public void setObligatedChangeIndirect(BigDecimal obligatedChangeIndirect) {
		this.obligatedChangeIndirect = obligatedChangeIndirect;
	}

	public BigDecimal getAnticipatedTotalDirect() {
		return anticipatedTotalDirect;
	}

	public void setAnticipatedTotalDirect(BigDecimal anticipatedTotalDirect) {
		this.anticipatedTotalDirect = anticipatedTotalDirect;
	}

	public BigDecimal getAnticipatedTotalIndirect() {
		return anticipatedTotalIndirect;
	}

	public void setAnticipatedTotalIndirect(BigDecimal anticipatedTotalIndirect) {
		this.anticipatedTotalIndirect = anticipatedTotalIndirect;
	}

	public BigDecimal getAnticipatedTotalAmount() {
		return anticipatedTotalAmount;
	}

	public void setAnticipatedTotalAmount(BigDecimal anticipatedTotalAmount) {
		this.anticipatedTotalAmount = anticipatedTotalAmount;
	}

	public BigDecimal getObligatedTotalIndirect() {
		return obligatedTotalIndirect;
	}

	public void setObligatedTotalIndirect(BigDecimal obligatedTotalIndirect) {
		this.obligatedTotalIndirect = obligatedTotalIndirect;
	}

	public BigDecimal getAmountObligatedToDate() {
		return amountObligatedToDate;
	}

	public void setAmountObligatedToDate(BigDecimal amountObligatedToDate) {
		this.amountObligatedToDate = amountObligatedToDate;
	}

	public BigDecimal getAntDistributableAmount() {
		return antDistributableAmount;
	}

	public void setAntDistributableAmount(BigDecimal antDistributableAmount) {
		this.antDistributableAmount = antDistributableAmount;
	}

	public BigDecimal getObliDistributableAmount() {
		return obliDistributableAmount;
	}

	public void setObliDistributableAmount(BigDecimal obliDistributableAmount) {
		this.obliDistributableAmount = obliDistributableAmount;
	}

	public Timestamp getFinalExpirationDate() {
		return finalExpirationDate;
	}

	public void setFinalExpirationDate(Timestamp finalExpirationDate) {
		this.finalExpirationDate = finalExpirationDate;
	}

	public Timestamp getCurrentFundEffectiveDate() {
		return currentFundEffectiveDate;
	}

	public void setCurrentFundEffectiveDate(Timestamp currentFundEffectiveDate) {
		this.currentFundEffectiveDate = currentFundEffectiveDate;
	}

	public Timestamp getObligationExpirationDate() {
		return obligationExpirationDate;
	}

	public void setObligationExpirationDate(Timestamp obligationExpirationDate) {
		this.obligationExpirationDate = obligationExpirationDate;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public BigDecimal getObligatedTotalDirect() {
		return obligatedTotalDirect;
	}

	public void setObligatedTotalDirect(BigDecimal obligatedTotalDirect) {
		this.obligatedTotalDirect = obligatedTotalDirect;
	}

	public BigDecimal getTotalCostInCurrency() {
		return totalCostInCurrency;
	}

	public void setTotalCostInCurrency(BigDecimal totalCostInCurrency) {
		this.totalCostInCurrency = totalCostInCurrency;
	}

	public String getCurrencyCode() {
		return currencyCode;
	}

	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}

	public Currency getCurrency() {
		return currency;
	}

	public void setCurrency(Currency currency) {
		this.currency = currency;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

}
