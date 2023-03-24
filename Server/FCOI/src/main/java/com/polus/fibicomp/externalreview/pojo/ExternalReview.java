package com.polus.fibicomp.externalreview.pojo;

import com.polus.fibicomp.person.pojo.Person;
import org.springframework.data.annotation.CreatedBy;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;

@Entity
@Table(name = "EXT_REVIEW")
public class ExternalReview implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "EXT_REVIEW_ID")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer extReviewID;

    @Column(name = "FIBI_REVIEW_ID")
    private Integer fibiReviewId;

    @Column(name = "MODULE_ITEM_CODE")
    private Integer moduleItemCode;

    @Column(name = "MODULE_SUB_ITEM_CODE")
    private Integer moduleSubItemCode;

    @Column(name = "MODULE_ITEM_KEY")
    private String moduleItemKey;

    @Column(name = "MODULE_SUB_ITEM_KEY")
    private String moduleSubItemKey;

    @Column(name = "REVIEW_MODULE_CODE")
    private Integer reviewModuleCode;

    @Column(name = "EXT_REVIEW_SERVICE_TYPE_CODE")
    private String extReviewServiceTypeCode;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(foreignKey = @ForeignKey(name = "EXT_REVIEW_FK_01"), name = "EXT_REVIEW_SERVICE_TYPE_CODE", insertable = false, updatable = false)
    private ExtReviewServiceType extReviewServiceType;

    @Column(name = "DESCRIPTION", nullable = false)
    private String description;

    @Column(name = "EXT_REVIEW_STATUS_CODE")
    private Integer extReviewStatusCode;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(foreignKey = @ForeignKey(name = "EXT_REVIEW_FK_02"), name = "EXT_REVIEW_STATUS_CODE", insertable = false, updatable = false)
    private ExtReviewStatus extReviewStatus;

    @CreatedBy
    @Column(name = "REVIEW_REQUESTOR_ID")
    private String reviewRequestorId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(foreignKey = @ForeignKey(name = "EXT_REVIEW_FK_03"), name = "REVIEW_REQUESTOR_ID", referencedColumnName = "PERSON_ID", insertable = false, updatable = false)
    private Person person;

    @Column(name = "REQUEST_DATE")
    private Timestamp requestDate;

    @Column(name = "DEADLINE_DATE")
    private Timestamp deadlineDate;

    @Column(name = "COMPLETION_DATE")
    private Timestamp completionDate;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    @Column(name = "PRE_PROPOSAL_EXT_REVIEW_ID")
    private Integer preProposalExtReviewId;

	@Transient
	private Boolean isTypeChange;

    public Integer getExtReviewID() {
        return extReviewID;
    }

    public void setExtReviewID(Integer extReviewID) {
        this.extReviewID = extReviewID;
    }

    public Integer getFibiReviewId() {
        return fibiReviewId;
    }

    public void setFibiReviewId(Integer fibiReviewId) {
        this.fibiReviewId = fibiReviewId;
    }

    public Integer getModuleItemCode() {
        return moduleItemCode;
    }

    public void setModuleItemCode(Integer moduleItemCode) {
        this.moduleItemCode = moduleItemCode;
    }

    public Integer getModuleSubItemCode() {
        return moduleSubItemCode;
    }

    public void setModuleSubItemCode(Integer moduleSubItemCode) {
        this.moduleSubItemCode = moduleSubItemCode;
    }

    public String getModuleItemKey() {
        return moduleItemKey;
    }

    public void setModuleItemKey(String moduleItemKey) {
        this.moduleItemKey = moduleItemKey;
    }

    public String getModuleSubItemKey() {
        return moduleSubItemKey;
    }

    public void setModuleSubItemKey(String moduleSubItemKey) {
        this.moduleSubItemKey = moduleSubItemKey;
    }

    public String getExtReviewServiceTypeCode() {
        return extReviewServiceTypeCode;
    }

    public void setExtReviewServiceTypeCode(String extReviewServiceTypeCode) {
        this.extReviewServiceTypeCode = extReviewServiceTypeCode;
    }

    public ExtReviewServiceType getExtReviewServiceType() {
        return extReviewServiceType;
    }

    public void setExtReviewServiceType(ExtReviewServiceType extReviewServiceType) {
        this.extReviewServiceType = extReviewServiceType;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Integer getExtReviewStatusCode() {
        return extReviewStatusCode;
    }

    public void setExtReviewStatusCode(Integer extReviewStatusCode) {
        this.extReviewStatusCode = extReviewStatusCode;
    }

    public ExtReviewStatus getExtReviewStatus() {
        return extReviewStatus;
    }

    public void setExtReviewStatus(ExtReviewStatus extReviewStatus) {
        this.extReviewStatus = extReviewStatus;
    }

    public String getReviewRequestorId() {
        return reviewRequestorId;
    }

    public void setReviewRequestorId(String reviewRequestorId) {
        this.reviewRequestorId = reviewRequestorId;
    }

    public Person getPerson() {
        return person;
    }

    public void setPerson(Person person) {
        this.person = person;
    }

    public Timestamp getRequestDate() {
        return requestDate;
    }

    public void setRequestDate(Timestamp requestDate) {
        this.requestDate = requestDate;
    }

    public Timestamp getDeadlineDate() {
        return deadlineDate;
    }

    public void setDeadlineDate(Timestamp deadlineDate) {
        this.deadlineDate = deadlineDate;
    }

    public Timestamp getCompletionDate() {
        return completionDate;
    }

    public void setCompletionDate(Timestamp completionDate) {
        this.completionDate = completionDate;
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

    public Integer getReviewModuleCode() {
        return reviewModuleCode;
    }

    public void setReviewModuleCode(Integer reviewModuleCode) {
        this.reviewModuleCode = reviewModuleCode;
    }

	public Boolean getIsTypeChange() {
		return isTypeChange;
	}

	public void setIsTypeChange(Boolean isTypeChange) {
		this.isTypeChange = isTypeChange;
	}

	public Integer getPreProposalExtReviewId() {
		return preProposalExtReviewId;
	}

	public void setPreProposalExtReviewId(Integer preProposalExtReviewId) {
		this.preProposalExtReviewId = preProposalExtReviewId;
	}

}
