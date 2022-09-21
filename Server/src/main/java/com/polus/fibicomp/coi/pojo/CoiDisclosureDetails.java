package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonManagedReference;


@Entity
@Table(name = "COI_DISCLOSURE_DETAILS")
@EntityListeners(AuditingEntityListener.class)
public class CoiDisclosureDetails implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "DISCLOSURE_DETAILS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer disclosureDetailsId;
	
	@Column(name = "DISCLOSURE_ID")
	private Integer disclosureId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE_DETAILS_FK1"), name = "DISCLOSURE_ID", referencedColumnName = "DISCLOSURE_ID", insertable = false, updatable = false)
	private CoiDisclosure coiDisclosure;
	
	@Column(name = "DISC_DET_STATUS_CODE")
	private String discDetStatusCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE_DETAILS_FK2"), name = "DISC_DET_STATUS_CODE", referencedColumnName = "DISC_DET_STATUS_CODE", insertable = false, updatable = false)
	private CoiDisclosureDetailsStatus coiDisclosureDetailsStatus;

	@Column(name = "COI_FINANCIAL_ENTITY_ID")
	private Integer coiFinancialEntityId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE_DETAILS_FK3"), name = "COI_FINANCIAL_ENTITY_ID", referencedColumnName = "COI_FINANCIAL_ENTITY_ID", insertable = false, updatable = false)
	private COIFinancialEntity coiFinancialEntity;
	
	@Column(name = "DISCLOSURE_NUMBER")
	private String disclosureNumber;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;
	
	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "COI_REVIEWER_STATUS_CODE")
	private String coiReviewerStatusCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE_DETAILS_FK4"), name = "COI_REVIEWER_STATUS_CODE", referencedColumnName = "DISC_DET_STATUS_CODE", insertable = false, updatable = false)
	private CoiDisclosureDetailsStatus coiReviewerStatus;

	@JsonManagedReference
	@OneToOne(mappedBy = "coiDisclosureDetails", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private CoiDisclosureDetailsComments comment;

	public Integer getDisclosureDetailsId() {
		return disclosureDetailsId;
	}

	public void setDisclosureDetailsId(Integer disclosureDetailsId) {
		this.disclosureDetailsId = disclosureDetailsId;
	}

	public Integer getDisclosureId() {
		return disclosureId;
	}

	public void setDisclosureId(Integer disclosureId) {
		this.disclosureId = disclosureId;
	}

	public CoiDisclosure getCoiDisclosure() {
		return coiDisclosure;
	}

	public void setCoiDisclosure(CoiDisclosure coiDisclosure) {
		this.coiDisclosure = coiDisclosure;
	}

	public String getDiscDetStatusCode() {
		return discDetStatusCode;
	}

	public void setDiscDetStatusCode(String discDetStatusCode) {
		this.discDetStatusCode = discDetStatusCode;
	}

	public CoiDisclosureDetailsStatus getCoiDisclosureDetailsStatus() {
		return coiDisclosureDetailsStatus;
	}

	public void setCoiDisclosureDetailsStatus(CoiDisclosureDetailsStatus coiDisclosureDetailsStatus) {
		this.coiDisclosureDetailsStatus = coiDisclosureDetailsStatus;
	}

	public Integer getCoiFinancialEntityId() {
		return coiFinancialEntityId;
	}

	public void setCoiFinancialEntityId(Integer coiFinancialEntityId) {
		this.coiFinancialEntityId = coiFinancialEntityId;
	}

	public COIFinancialEntity getCoiFinancialEntity() {
		return coiFinancialEntity;
	}

	public void setCoiFinancialEntity(COIFinancialEntity coiFinancialEntity) {
		this.coiFinancialEntity = coiFinancialEntity;
	}

	public String getDisclosureNumber() {
		return disclosureNumber;
	}

	public void setDisclosureNumber(String disclosureNumber) {
		this.disclosureNumber = disclosureNumber;
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

	public CoiDisclosureDetailsComments getComment() {
		return comment;
	}

	public void setComment(CoiDisclosureDetailsComments comment) {
		this.comment = comment;
	}

	public String getCoiReviewerStatusCode() {
		return coiReviewerStatusCode;
	}

	public void setCoiReviewerStatusCode(String coiReviewerStatusCode) {
		this.coiReviewerStatusCode = coiReviewerStatusCode;
	}

	public CoiDisclosureDetailsStatus getCoiReviewerStatus() {
		return coiReviewerStatus;
	}

	public void setCoiReviewerStatus(CoiDisclosureDetailsStatus coiReviewerStatus) {
		this.coiReviewerStatus = coiReviewerStatus;
	}

}
