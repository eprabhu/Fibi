package com.polus.fibicomp.orcid.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
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

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "ORCID_WORK_TYPE")
@IdClass(OrcidWorkType.OrcidWorkTypeId.class)
public class OrcidWorkType implements Serializable, Comparable<OrcidWorkType> {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ORCID_WORK_TYPE_CODE")
	private String orcidWorkTypeCode;

	@Id
	@Column(name = "ORCID_WORK_CATEGORY_CODE")
	private String orcidWorkCategoryCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "ORCID_WORK_TYPE_FK1"), name = "ORCID_WORK_CATEGORY_CODE", referencedColumnName = "ORCID_WORK_CATEGORY_CODE", insertable = false, updatable = false)
	private OrcidWorkCategory orcidWorkCategory;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "SORT_ORDER")
	private Integer sortOrder;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private boolean isActive = false;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getOrcidWorkTypeCode() {
		return orcidWorkTypeCode;
	}

	public void setOrcidWorkTypeCode(String orcidWorkTypeCode) {
		this.orcidWorkTypeCode = orcidWorkTypeCode;
	}

	public String getOrcidWorkCategoryCode() {
		return orcidWorkCategoryCode;
	}

	public void setOrcidWorkCategoryCode(String orcidWorkCategoryCode) {
		this.orcidWorkCategoryCode = orcidWorkCategoryCode;
	}

	public OrcidWorkCategory getOrcidWorkCategory() {
		return orcidWorkCategory;
	}

	public void setOrcidWorkCategory(OrcidWorkCategory orcidWorkCategory) {
		this.orcidWorkCategory = orcidWorkCategory;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Integer getSortOrder() {
		return sortOrder;
	}

	public void setSortOrder(Integer sortOrder) {
		this.sortOrder = sortOrder;
	}

	public boolean isActive() {
		return isActive;
	}

	public void setActive(boolean isActive) {
		this.isActive = isActive;
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

	@Override
	public int compareTo(OrcidWorkType orcidWorkType) {
		return new CompareToBuilder().append(this.orcidWorkCategoryCode, orcidWorkType.orcidWorkCategoryCode).append(this.orcidWorkTypeCode, orcidWorkType.orcidWorkTypeCode).toComparison();
	}

	public static final class OrcidWorkTypeId implements Serializable, Comparable<OrcidWorkTypeId> {

		private static final long serialVersionUID = 1L;

		private String orcidWorkTypeCode;

		private String orcidWorkCategoryCode;

		public String getOrcidWorkTypeCode() {
			return orcidWorkTypeCode;
		}

		public void setOrcidWorkTypeCode(String orcidWorkTypeCode) {
			this.orcidWorkTypeCode = orcidWorkTypeCode;
		}

		public String getOrcidWorkCategoryCode() {
			return orcidWorkCategoryCode;
		}

		public void setOrcidWorkCategoryCode(String orcidWorkCategoryCode) {
			this.orcidWorkCategoryCode = orcidWorkCategoryCode;
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("orcidWorkCategoryCode", this.orcidWorkCategoryCode).append("orcidWorkTypeCode", this.orcidWorkTypeCode).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final OrcidWorkTypeId rhs = (OrcidWorkTypeId) other;
			return new EqualsBuilder().append(this.orcidWorkCategoryCode, rhs.orcidWorkCategoryCode).append(this.orcidWorkTypeCode, rhs.orcidWorkTypeCode).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.orcidWorkCategoryCode).append(this.orcidWorkTypeCode).toHashCode();
		}

		@Override
		public int compareTo(OrcidWorkTypeId other) {
			return new CompareToBuilder().append(this.orcidWorkCategoryCode, other.orcidWorkCategoryCode).append(this.orcidWorkTypeCode, other.orcidWorkCategoryCode).toComparison();
		}

	}

}
