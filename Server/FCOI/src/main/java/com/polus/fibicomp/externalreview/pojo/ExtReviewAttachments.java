package com.polus.fibicomp.externalreview.pojo;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;

@Entity
@Table(name = "EXT_REVIEW_ATTACHMENTS")
public class ExtReviewAttachments implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "EXT_REVIEW_ATTACHMENT_ID")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer extReviewAttachmentId;

    @Column(name = "EXT_REVIEW_ID")
    private Integer extReviewID;

    @ManyToOne( fetch = FetchType.LAZY)
    @JoinColumn(foreignKey = @ForeignKey(name = "EXT_REVIEW_ATTACH_FK_01"), name = "EXT_REVIEW_ID", insertable = false, updatable = false)
    private ExternalReview externalReview;

    @Column(name = "DESCRIPTION", nullable = false)
    private String description;

    @Column(name = "FILE_NAME", nullable = false)
    private String fileName;

    @Column(name = "ATTACHMENT_TYPE_CODE", nullable = false)
    private Integer attachmentTypeCode;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(foreignKey = @ForeignKey(name = "EXT_REVIEW_ATTACH_FK_02"), name = "ATTACHMENT_TYPE_CODE", insertable = false, updatable = false)
    private ExtReviewAttachmentType extReviewAttachmentType;

    @Column(name = "FILE_DATA_ID")
    private String fileDataId;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(foreignKey = @ForeignKey(name = "EXT_REVIEW_ATTACH_FK_03"), name = "FILE_DATA_ID", insertable = false, updatable = false)
    private ExtReviewAttachmentFile attachmentFile;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    @Column(name = "IS_ATTACHMENT_MANDATORY")
    private String isAttachmentMandatory;
    
    @Transient
	private String lastUpdateUserFullName;

    public Integer getExtReviewAttachmentId() {
        return extReviewAttachmentId;
    }

    public void setExtReviewAttachmentId(Integer extReviewAttachmentId) {
        this.extReviewAttachmentId = extReviewAttachmentId;
    }

    public Integer getExtReviewID() {
        return extReviewID;
    }

    public void setExtReviewID(Integer extReviewID) {
        this.extReviewID = extReviewID;
    }

    public ExternalReview getExternalReview() {
        return externalReview;
    }

    public void setExternalReview(ExternalReview externalReview) {
        this.externalReview = externalReview;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public Integer getAttachmentTypeCode() {
        return attachmentTypeCode;
    }

    public void setAttachmentTypeCode(Integer attachmentTypeCode) {
        this.attachmentTypeCode = attachmentTypeCode;
    }

    public ExtReviewAttachmentType getExtReviewAttachmentType() {
        return extReviewAttachmentType;
    }

    public void setExtReviewAttachmentType(ExtReviewAttachmentType extReviewAttachmentType) {
        this.extReviewAttachmentType = extReviewAttachmentType;
    }

    public String getFileDataId() {
        return fileDataId;
    }

    public void setFileDataId(String fileDataId) {
        this.fileDataId = fileDataId;
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

    public ExtReviewAttachmentFile getAttachmentFile() {
        return attachmentFile;
    }

    public void setAttachmentFile(ExtReviewAttachmentFile attachmentFile) {
        this.attachmentFile = attachmentFile;
    }

	public String getIsAttachmentMandatory() {
		return isAttachmentMandatory;
	}

	public void setIsAttachmentMandatory(String isAttachmentMandatory) {
		this.isAttachmentMandatory = isAttachmentMandatory;
	}

	public String getLastUpdateUserFullName() {
		return lastUpdateUserFullName;
	}

	public void setLastUpdateUserFullName(String lastUpdateUserFullName) {
		this.lastUpdateUserFullName = lastUpdateUserFullName;
	}

}
