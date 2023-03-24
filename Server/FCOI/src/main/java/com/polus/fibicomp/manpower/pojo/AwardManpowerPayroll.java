package com.polus.fibicomp.manpower.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.award.expense.pojo.AwardExpenseTransaction;

@Entity
@Table(name = "AWARD_MANPOWER_PAYROLL")
public class AwardManpowerPayroll implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PAYROLL_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_PAYROLL_ID_GENERATOR")
	@SequenceGenerator(name="SEQ_PAYROLL_ID_GENERATOR", sequenceName = "SEQ_PAYROLL_ID_GENERATOR", allocationSize=1)
	private Integer payrollId;

	@Column(name = "GL_ACCOUNT_CODE")
	private String glAccountCode;

	@Column(name = "EMPLOYEE_NUMBER")
	private String employeeNumber;

	@Column(name = "INTERNAL_ORDER_CODE")
	private String internalOrderCode;

	@Column(name = "COST_SHARING",precision = 5, scale = 2)
	private BigDecimal costSharing;

	@Column(name = "PAY_ELEMENT_CODE")
	private String payElementCode;

	@Column(name = "PAY_ELEMENT")
	private String payElement;
	
	@Column(name = "AMOUNT")
	private String amount;	

	@Column(name = "PAYROLL_PERIOD")
	private String payrollPeriod;
	
	@Column(name = "FILE_ID")
	private Integer fileId;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;	
	
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "REMARKS")
	private String remarks;

	@Transient
	private List<AwardExpenseTransaction> awardExpenseTransaction;	

	@Transient
	private String payrollAmount;	

	public AwardManpowerPayroll() {
		
	}

	public BigDecimal getCostSharing() {
		return costSharing;
	}

	public void setCostSharing(BigDecimal costSharing) {
		this.costSharing = costSharing;
	}

	public String getRemarks() {
		return remarks;
	}

	public void setRemarks(String remarks) {
		this.remarks = remarks;
	}

	public String getAmount() {
		return amount;
	}

	public void setAmount(String amount) {
		this.amount = amount;
	}

	public Integer getFileId() {
		return fileId;
	}

	public void setFileId(Integer fileId) {
		this.fileId = fileId;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}
	
	public Integer getPayrollId() {
		return payrollId;
	}

	public void setPayrollId(Integer payrollId) {
		this.payrollId = payrollId;
	}	

	public String getGlAccountCode() {
		return glAccountCode;
	}

	public void setGlAccountCode(String glAccountCode) {
		this.glAccountCode = glAccountCode;
	}

	public String getEmployeeNumber() {
		return employeeNumber;
	}

	public void setEmployeeNumber(String employeeNumber) {
		this.employeeNumber = employeeNumber;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}


	public String getPayElementCode() {
		return payElementCode;
	}

	public void setPayElementCode(String payElementCode) {
		this.payElementCode = payElementCode;
	}

	public String getPayElement() {
		return payElement;
	}

	public void setPayElement(String payElement) {
		this.payElement = payElement;
	}
	

	public String getPayrollPeriod() {
		return payrollPeriod;
	}

	public void setPayrollPeriod(String payrollPeriod) {
		this.payrollPeriod = payrollPeriod;
	}

	public List<AwardExpenseTransaction> getAwardExpenseTransaction() {
		return awardExpenseTransaction;
	}

	public void setAwardExpenseTransaction(List<AwardExpenseTransaction> awardExpenseTransaction) {
		this.awardExpenseTransaction = awardExpenseTransaction;
	}

	public AwardManpowerPayroll(String glAccountCode, String employeeNumber,
			String internalOrderCode, BigDecimal costSharing, String payElementCode, String payElement, String amount,
			String payrollPeriod) {
		super();
		
		this.glAccountCode = glAccountCode;
		this.employeeNumber = employeeNumber;
		this.internalOrderCode = internalOrderCode;
		this.costSharing = costSharing;
		this.payElementCode = payElementCode;
		this.payElement = payElement;
		this.amount = amount;
		this.payrollPeriod = payrollPeriod;		
	}

	public String getPayrollAmount() {
		return payrollAmount;
	}

	public void setPayrollAmount(String payrollAmount) {
		this.payrollAmount = payrollAmount;
	}
	
}
