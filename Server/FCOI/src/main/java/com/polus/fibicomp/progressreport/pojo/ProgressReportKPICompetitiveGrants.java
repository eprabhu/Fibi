package com.polus.fibicomp.progressreport.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;

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

import com.polus.fibicomp.pojo.Sponsor;

@Entity
@Table(name = "PROGRESS_REPORT_KPI_COMPETITIVE_GRANTS")
@EntityListeners(AuditingEntityListener.class)
public class ProgressReportKPICompetitiveGrants implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "KPI_COMPETITIVE_GRANTS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer kpiCompetitiveGrantsId;
	
	@Column(name = "KPI_SUMMARY_ID")
	private Integer kpiSummaryId;
	
	@Column(name = "PROGRESS_REPORT_ID")
	private Integer progressReportId;
	
	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaCode;
	
	@Column(name = "PROJECT_TITLE")
	private String projectTitle;
	
	@Column(name = "PROJECT_DESCRIPTION")
	private String projectDescription;
	
	@Column(name = "PROJECT_START_DATE")
	private Date projectStartDate;
	
	@Column(name = "PROJECT_END_DATE")
	private Date projectEndDate;
	
	@Column(name = "NAME_OF_GRANT_RECEIVED")
	private String nameOfGrantReceived;
	
	@Column(name = "PROJECT_REFERENCE_NO")
	private String projectReferenceNo;
	
	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;
	
	@Column(name = "RECIPIENT_OF_GRANT")
	private String recipientOfGrant;
	
	@Column(name = "HOST_INSTITUTION")
	private String hostInsitution;
	
	@Column(name = "DIRECT_COST")
	private BigDecimal directCost;
	
	@Column(name = "INDIRECT_COST")
	private BigDecimal indirectCost;
	
	@Column(name = "COMMENTS")
	private String comments;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "KPI_COMPETITIVE_GRANTS_FK1"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	public Integer getKpiCompetitiveGrantsId() {
		return kpiCompetitiveGrantsId;
	}

	public void setKpiCompetitiveGrantsId(Integer kpiCompetitiveGrantsId) {
		this.kpiCompetitiveGrantsId = kpiCompetitiveGrantsId;
	}

	public Integer getKpiSummaryId() {
		return kpiSummaryId;
	}

	public void setKpiSummaryId(Integer kpiSummaryId) {
		this.kpiSummaryId = kpiSummaryId;
	}

	public Integer getProgressReportId() {
		return progressReportId;
	}

	public void setProgressReportId(Integer progressReportId) {
		this.progressReportId = progressReportId;
	}

	public String getKpiCriteriaCode() {
		return kpiCriteriaCode;
	}

	public void setKpiCriteriaCode(String kpiCriteriaCode) {
		this.kpiCriteriaCode = kpiCriteriaCode;
	}

	public String getProjectTitle() {
		return projectTitle;
	}

	public void setProjectTitle(String projectTitle) {
		this.projectTitle = projectTitle;
	}

	public String getProjectDescription() {
		return projectDescription;
	}

	public void setProjectDescription(String projectDescription) {
		this.projectDescription = projectDescription;
	}

	public Date getProjectStartDate() {
		return projectStartDate;
	}

	public void setProjectStartDate(Date projectStartDate) {
		this.projectStartDate = projectStartDate;
	}

	public Date getProjectEndDate() {
		return projectEndDate;
	}

	public void setProjectEndDate(Date projectEndDate) {
		this.projectEndDate = projectEndDate;
	}

	public String getNameOfGrantReceived() {
		return nameOfGrantReceived;
	}

	public void setNameOfGrantReceived(String nameOfGrantReceived) {
		this.nameOfGrantReceived = nameOfGrantReceived;
	}

	public String getProjectReferenceNo() {
		return projectReferenceNo;
	}

	public void setProjectReferenceNo(String projectReferenceNo) {
		this.projectReferenceNo = projectReferenceNo;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public String getRecipientOfGrant() {
		return recipientOfGrant;
	}

	public void setRecipientOfGrant(String recipientOfGrant) {
		this.recipientOfGrant = recipientOfGrant;
	}

	public String getHostInsitution() {
		return hostInsitution;
	}

	public void setHostInsitution(String hostInsitution) {
		this.hostInsitution = hostInsitution;
	}

	public BigDecimal getDirectCost() {
		return directCost;
	}

	public void setDirectCost(BigDecimal directCost) {
		this.directCost = directCost;
	}

	public BigDecimal getIndirectCost() {
		return indirectCost;
	}

	public void setIndirectCost(BigDecimal indirectCost) {
		this.indirectCost = indirectCost;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
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

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
	}
}
