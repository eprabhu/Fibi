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

import com.polus.fibicomp.roles.pojo.Rights;

@Entity
@Table(name = "RIGHT_SECTION_MAPPING")
@IdClass(RightSectionMapping.RightSectionMappingId.class)
public class RightSectionMapping implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SECTION_CODE")
	private String sectionCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "RIGHT_SECTION_MAPPING_FK2"), name = "SECTION_CODE", referencedColumnName = "SECTION_CODE", insertable = false, updatable = false)
	private AgreementSectionType sectionType;

	@Id
	@Column(name = "RIGHT_ID")
	private Integer rightId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "RIGHT_SECTION_MAPPING_FK1"), name = "RIGHT_ID", referencedColumnName = "RIGHT_ID", insertable = false, updatable = false)
	private Rights rights;

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

	public void setSectionType(AgreementSectionType sectionType) {
		this.sectionType = sectionType;
	}

	public static final class RightSectionMappingId implements Serializable {

		private static final long serialVersionUID = 1L;

		private String sectionCode;

		private Integer rightId;

		public String getSectionCode() {
			return sectionCode;
		}

		public void setSectionCode(String sectionCode) {
			this.sectionCode = sectionCode;
		}
	
		public static long getSerialversionuid() {
			return serialVersionUID;
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("sectionCode", this.sectionCode)
					.append("rightId", this.rightId).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final RightSectionMappingId rhs = (RightSectionMappingId) other;
			return new EqualsBuilder().append(this.sectionCode, rhs.sectionCode)
					.append(this.rightId, rhs.rightId).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.sectionCode).append(this.rightId).toHashCode();
		}

		public int compareTo(RightSectionMappingId other) {
			return new CompareToBuilder().append(this.sectionCode, other.sectionCode)
					.append(this.rightId, other.rightId).toComparison();
		}

		public Integer getRightId() {
			return rightId;
		}

		public void setRightId(Integer rightId) {
			this.rightId = rightId;
		}

	}

	public Rights getRights() {
		return rights;
	}

	public void setRights(Rights rights) {
		this.rights = rights;
	}

	public AgreementSectionType getSectionType() {
		return sectionType;
	}

	public Integer getRightId() {
		return rightId;
	}

	public void setRightId(Integer rightId) {
		this.rightId = rightId;
	}
}
