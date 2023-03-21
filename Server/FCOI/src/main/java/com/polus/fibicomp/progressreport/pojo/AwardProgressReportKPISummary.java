package com.polus.fibicomp.progressreport.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.grantcall.pojo.KPICriteriaType;
import com.polus.fibicomp.grantcall.pojo.KPIType;

@Entity
@Table(name = "AWARD_PROGRESS_REPORT_KPI_SUMMARY")
@EntityListeners(AuditingEntityListener.class)
public class AwardProgressReportKPISummary implements Serializable {

	private static final long serialVersionUID = 1L;
	
	@Id
	@Column(name = "KPI_SUMMARY_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer kpiSummaryId;
	
	@Column(name = "AWARD_ID")
	private Integer awardId;
	
	@Column(name = "AWARD_NUMBER")
	private String awardNumber;
	
	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;
	
	@Column(name = "KPI_CATEGORY_TYPE_CODE")
	private String kpiCategoryTypeCode;
	
	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaTypeCode;
	
	@Column(name = "TARGET")
	private Integer target;
	
	@Column(name = "ACHIEVED")
	private BigDecimal achieved = BigDecimal.ZERO;
	
	@Column(name = "SECTION_CODE")
	private String sectionCode;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Date updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "KPI_SUMMARY_FK1"), name = "KPI_CRITERIA_TYPE_CODE", referencedColumnName = "KPI_CRITERIA_TYPE_CODE", insertable = false, updatable = false)
	private KPICriteriaType kpiCriteriaType;	

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "KPI_SUMMARY_FK3"), name = "KPI_CATEGORY_TYPE_CODE", referencedColumnName = "KPI_TYPE_CODE", insertable = false, updatable = false)
	private KPIType kpiType;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "KPI_SUMMARY_FK2"), name = "PROGRESS_REPORT_ID", referencedColumnName = "PROGRESS_REPORT_ID")
	private AwardProgressReport awardProgressReport;

	@Column(name = "ORGINATING_PROGRESS_REPORT_ID")
	private Integer originatingProgressReportId;
	
	private transient Map<Object, Object> achievedValues;
	
	private transient Double totalAchieved;

	public Integer getKpiSummaryId() {
		return kpiSummaryId;
	}

	public void setKpiSummaryId(Integer kpiSummaryId) {
		this.kpiSummaryId = kpiSummaryId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
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

	public String getKpiCategoryTypeCode() {
		return kpiCategoryTypeCode;
	}

	public void setKpiCategoryTypeCode(String kpiCategoryTypeCode) {
		this.kpiCategoryTypeCode = kpiCategoryTypeCode;
	}

	public String getKpiCriteriaTypeCode() {
		return kpiCriteriaTypeCode;
	}

	public void setKpiCriteriaTypeCode(String kpiCriteriaTypeCode) {
		this.kpiCriteriaTypeCode = kpiCriteriaTypeCode;
	}

	public String getSectionCode() {
		return sectionCode;
	}

	public void setSectionCode(String sectionCode) {
		this.sectionCode = sectionCode;
	}

	public Date getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Date updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public KPICriteriaType getKpiCriteriaType() {
		return kpiCriteriaType;
	}

	public void setKpiCriteriaType(KPICriteriaType kpiCriteriaType) {
		this.kpiCriteriaType = kpiCriteriaType;
	}

	public AwardProgressReport getAwardProgressReport() {
		return awardProgressReport;
	}

	public void setAwardProgressReport(AwardProgressReport awardProgressReport) {
		this.awardProgressReport = awardProgressReport;
	}

	public KPIType getKpiType() {
		return kpiType;
	}

	public void setKpiType(KPIType kpiType) {
		this.kpiType = kpiType;
	}

	public Map<Object, Object> getAchievedValues() {
		return achievedValues;
	}

	public void setAchievedValues(Map<Object, Object> achievedValues) {
		this.achievedValues = achievedValues;
	}

	public Integer getTarget() {
		return target;
	}

	public void setTarget(Integer target) {
		this.target = target;
	}

	public BigDecimal getAchieved() {
		return achieved;
	}

	public void setAchieved(BigDecimal achieved) {
		this.achieved = achieved;
	}

	public Double getTotalAchieved() {
		return totalAchieved;
	}

	public void setTotalAchieved(Double totalAchieved) {
		this.totalAchieved = totalAchieved;
	}

	public Integer getOriginatingProgressReportId() {
		return originatingProgressReportId;
	}

	public void setOriginatingProgressReportId(Integer originatingProgressReportId) {
		this.originatingProgressReportId = originatingProgressReportId;
	}
}
