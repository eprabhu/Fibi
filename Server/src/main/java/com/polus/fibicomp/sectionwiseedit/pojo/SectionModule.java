package com.polus.fibicomp.sectionwiseedit.pojo;

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
@Table(name = "SECTION_MODULE")
@IdClass(SectionModule.SectionModuleId.class)
public class SectionModule implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SECTION_CODE")
	private String sectionCode;

	@Id
	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Id
	@Column(name = "SUB_MODULE_CODE")
	private Integer subModuleCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "SECTION_MODULE_FK1"), name = "SECTION_CODE", referencedColumnName = "SECTION_CODE", insertable = false, updatable = false)
	private SectionType sectionType;

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

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public SectionType getSectionType() {
		return sectionType;
	}

	public void setSectionType(SectionType sectionType) {
		this.sectionType = sectionType;
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

	public static final class SectionModuleId implements Serializable {

		private static final long serialVersionUID = 1L;

		private String sectionCode;

		private Integer moduleCode;

		private Integer subModuleCode;

		public String getSectionCode() {
			return sectionCode;
		}

		public void setSectionCode(String sectionCode) {
			this.sectionCode = sectionCode;
		}

		public Integer getModuleCode() {
			return moduleCode;
		}

		public void setModuleCode(Integer moduleCode) {
			this.moduleCode = moduleCode;
		}

		public Integer getSubModuleCode() {
			return subModuleCode;
		}

		public void setSubModuleCode(Integer subModuleCode) {
			this.subModuleCode = subModuleCode;
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("sectionCode", this.sectionCode)
					.append("moduleCode", this.moduleCode).append("subModuleCode", this.subModuleCode).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final SectionModuleId rhs = (SectionModuleId) other;
			return new EqualsBuilder().append(this.sectionCode, rhs.sectionCode)
					.append(this.moduleCode, rhs.moduleCode).append(this.subModuleCode, rhs.subModuleCode).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.sectionCode).append(this.moduleCode).append(this.subModuleCode).toHashCode();
		}

		public int compareTo(SectionModuleId other) {
			return new CompareToBuilder().append(this.sectionCode, other.sectionCode)
					.append(this.moduleCode, other.moduleCode).append(this.subModuleCode, other.subModuleCode).toComparison();
		}
	}
}
