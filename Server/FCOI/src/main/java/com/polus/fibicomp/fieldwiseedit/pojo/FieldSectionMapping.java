package com.polus.fibicomp.fieldwiseedit.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

@Entity
@Table(name = "FIELD_SECTION_MAPPING")
@IdClass(FieldSectionMapping.FieldSectionMappingId.class)
public class FieldSectionMapping implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SECTION_CODE")
	private String sectionCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "FIELD_SECTION_MAPPING_FK2"), name = "SECTION_CODE", referencedColumnName = "SECTION_CODE", insertable = false, updatable = false)
	private AgreementSectionType sectionType;

	@Id
	@Column(name = "FIELD_CODE")
	private String fieldCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "FIELD_SECTION_MAPPING_FK1"), name = "FIELD_CODE", referencedColumnName = "FIELD_CODE", insertable = false, updatable = false)
	private AgreementFieldType fieldType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getSectionCode() {
		return sectionCode;
	}

	public void setSectionCode(String sectionCode) {
		this.sectionCode = sectionCode;
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

	public String getFieldCode() {
		return fieldCode;
	}

	public void setFieldCode(String fieldCode) {
		this.fieldCode = fieldCode;
	}

	public AgreementFieldType getFieldType() {
		return fieldType;
	}

	public void setFieldType(AgreementFieldType fieldType) {
		this.fieldType = fieldType;
	}

	public void setSectionType(AgreementSectionType sectionType) {
		this.sectionType = sectionType;
	}

	public static final class FieldSectionMappingId implements Serializable {

		private static final long serialVersionUID = 1L;

		private String sectionCode;

		private String fieldCode;

		public String getSectionCode() {
			return sectionCode;
		}

		public void setSectionCode(String sectionCode) {
			this.sectionCode = sectionCode;
		}

		public String getFieldCode() {
			return fieldCode;
		}

		public void setFieldCode(String fieldCode) {
			this.fieldCode = fieldCode;
		}
	
		public static long getSerialversionuid() {
			return serialVersionUID;
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("sectionCode", this.sectionCode)
					.append("fieldCode", this.fieldCode).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final FieldSectionMappingId rhs = (FieldSectionMappingId) other;
			return new EqualsBuilder().append(this.sectionCode, rhs.sectionCode)
					.append(this.fieldCode, rhs.fieldCode).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.sectionCode).append(this.fieldCode).toHashCode();
		}

		public int compareTo(FieldSectionMappingId other) {
			return new CompareToBuilder().append(this.sectionCode, other.sectionCode)
					.append(this.fieldCode, other.fieldCode).toComparison();
		}

	}
}
