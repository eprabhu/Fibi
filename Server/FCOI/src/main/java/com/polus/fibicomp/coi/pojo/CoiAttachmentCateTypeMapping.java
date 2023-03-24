package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
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

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "COI_ATTA_CATE_TYPE_MAPPING")
@EntityListeners(AuditingEntityListener.class)
public class CoiAttachmentCateTypeMapping implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COI_ATTA_CATE_TYPE_MAPPING_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer coiAttachmentCateTypeMappingId;
	
	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "COI_ATTA_CATEGORY_TYPE_CODE")
	private String coiAttachmentCategoryTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_ATTA_CATE_TYPE_MAPPING_FK1"), name = "COI_ATTA_CATEGORY_TYPE_CODE", referencedColumnName = "COI_ATTA_CATEGORY_TYPE_CODE", insertable = false, updatable = false)
	private CoiAttachmentCategoryType coiAttachmentCategoryType;

	@Column(name = "COI_ATTA_TYPE_CODE")
	private String coiAttachmentTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_ATTA_CATE_TYPE_MAPPING_FK2"), name = "COI_ATTA_TYPE_CODE", referencedColumnName = "COI_ATTA_TYPE_CODE", insertable = false, updatable = false)
	private CoiAttachmentType coiAttachmentType;
	
	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getCoiAttachmentCateTypeMappingId() {
		return coiAttachmentCateTypeMappingId;
	}

	public void setCoiAttachmentCateTypeMappingId(Integer coiAttachmentCateTypeMappingId) {
		this.coiAttachmentCateTypeMappingId = coiAttachmentCateTypeMappingId;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getCoiAttachmentCategoryTypeCode() {
		return coiAttachmentCategoryTypeCode;
	}

	public void setCoiAttachmentCategoryTypeCode(String coiAttachmentCategoryTypeCode) {
		this.coiAttachmentCategoryTypeCode = coiAttachmentCategoryTypeCode;
	}

	public CoiAttachmentCategoryType getCoiAttachmentCategoryType() {
		return coiAttachmentCategoryType;
	}

	public void setCoiAttachmentCategoryType(CoiAttachmentCategoryType coiAttachmentCategoryType) {
		this.coiAttachmentCategoryType = coiAttachmentCategoryType;
	}

	public String getCoiAttachmentTypeCode() {
		return coiAttachmentTypeCode;
	}

	public void setCoiAttachmentTypeCode(String coiAttachmentTypeCode) {
		this.coiAttachmentTypeCode = coiAttachmentTypeCode;
	}

	public CoiAttachmentType getCoiAttachmentType() {
		return coiAttachmentType;
	}

	public void setCoiAttachmentType(CoiAttachmentType coiAttachmentType) {
		this.coiAttachmentType = coiAttachmentType;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
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

}
