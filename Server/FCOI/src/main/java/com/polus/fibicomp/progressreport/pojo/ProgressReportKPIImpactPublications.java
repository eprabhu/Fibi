package com.polus.fibicomp.progressreport.pojo;

import java.io.Serializable;
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

@Entity
@Table(name = "PROGRESS_REPORT_KPI_IMPACT_PUBLICATIONS")
@EntityListeners(AuditingEntityListener.class)
public class ProgressReportKPIImpactPublications implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "KPI_IMPACT_PUBLICATIONS_ID")
	private Integer kpiImpactPublicationId;

	@Column(name = "KPI_SUMMARY_ID")
	private Integer kpiSummaryId;

	@Column(name = "PROGRESS_REPORT_ID")
	private Integer progressReportId;

	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaCode;

	@Column(name = "PUBLICATION_STATUS_CODE")
	private String publicationStatusCode;

	@Column(name = "AUTHOR_NAME")
	private String authorName;

	@Column(name = "TITLE_OF_ARTICLE")
	private String titleOfArticle;

	@Column(name = "JOURNAL_NAME")
	private String journalName;

	@Column(name = "PUBLISHER")
	private String publisher;

	@Column(name = "YEAR")
	private String year;

	@Column(name = "PAGE_NO")
	private String pageNo;

	@Column(name = "IMPACT_FACTOR")
	private String impactFactor;

	@Column(name = "FUNDING_ACKNOWLEDGEMENT")
	private String fundingAcknowledgement;

	@Column(name = "COMMENTS")
	private String comments;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "PUT_CODE")
	private Integer putCode;

	@Column(name = "PUBLICATION_DATE")
	private Date publicationDate;
	
	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "KPI_IMPACT_PUBLICATIONS_FK1"), name = "PUBLICATION_STATUS_CODE", referencedColumnName = "PUBLICATION_STATUS_CODE", insertable = false, updatable = false)
	private KPIPublicationStatus kpiPublicationStatus;

	public Integer getKpiImpactPublicationId() {
		return kpiImpactPublicationId;
	}

	public void setKpiImpactPublicationId(Integer kpiImpactPublicationId) {
		this.kpiImpactPublicationId = kpiImpactPublicationId;
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

	public String getPublicationStatusCode() {
		return publicationStatusCode;
	}

	public void setPublicationStatusCode(String publicationStatusCode) {
		this.publicationStatusCode = publicationStatusCode;
	}

	public String getAuthorName() {
		return authorName;
	}

	public void setAuthorName(String authorName) {
		this.authorName = authorName;
	}

	public String getTitleOfArticle() {
		return titleOfArticle;
	}

	public void setTitleOfArticle(String titleOfArticle) {
		this.titleOfArticle = titleOfArticle;
	}

	public String getJournalName() {
		return journalName;
	}

	public void setJournalName(String journalName) {
		this.journalName = journalName;
	}

	public String getYear() {
		return year;
	}

	public void setYear(String year) {
		this.year = year;
	}

	public String getPageNo() {
		return pageNo;
	}

	public void setPageNo(String pageNo) {
		this.pageNo = pageNo;
	}

	public String getImpactFactor() {
		return impactFactor;
	}

	public void setImpactFactor(String impactFactor) {
		this.impactFactor = impactFactor;
	}

	public String getFundingAcknowledgement() {
		return fundingAcknowledgement;
	}

	public void setFundingAcknowledgement(String fundingAcknowledgement) {
		this.fundingAcknowledgement = fundingAcknowledgement;
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

	public KPIPublicationStatus getKpiPublicationStatus() {
		return kpiPublicationStatus;
	}

	public void setKpiPublicationStatus(KPIPublicationStatus kpiPublicationStatus) {
		this.kpiPublicationStatus = kpiPublicationStatus;
	}

	public String getPublisher() {
		return publisher;
	}

	public void setPublisher(String publisher) {
		this.publisher = publisher;
	}

	public Integer getPutCode() {
		return putCode;
	}

	public void setPutCode(Integer putCode) {
		this.putCode = putCode;
	}

	public Date getPublicationDate() {
		return publicationDate;
	}

	public void setPublicationDate(Date publicationDate) {
		this.publicationDate = publicationDate;
	}
}
