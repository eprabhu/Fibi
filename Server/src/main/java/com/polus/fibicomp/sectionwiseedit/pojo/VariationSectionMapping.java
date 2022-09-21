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

import com.polus.fibicomp.servicerequest.pojo.ServiceRequestType;

@Entity
@Table(name = "VARIATION_SECTION_MAPPING")
@IdClass(VariationSectionMapping.VariationSectionMappingId.class)
public class VariationSectionMapping implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SECTION_CODE")
	private String sectionCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "VARIATION_SECTION_MAPPING_FK2"), name = "SECTION_CODE", referencedColumnName = "SECTION_CODE", insertable = false, updatable = false)
	private SectionType sectionType;

	@Id
	@Column(name = "TYPE_CODE")
	private String typeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "VARIATION_SECTION_MAPPING_FK1"), name = "TYPE_CODE", referencedColumnName = "TYPE_CODE", insertable = false, updatable = false)
	private ServiceRequestType serviceRequestType;

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

	public static final class VariationSectionMappingId implements Serializable {

		private static final long serialVersionUID = 1L;

		private String sectionCode;

		private Integer typeCode;

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
					.append("typeCode", this.typeCode).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final VariationSectionMappingId rhs = (VariationSectionMappingId) other;
			return new EqualsBuilder().append(this.sectionCode, rhs.sectionCode)
					.append(this.typeCode, rhs.typeCode).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.sectionCode).append(this.typeCode).toHashCode();
		}

		public int compareTo(VariationSectionMappingId other) {
			return new CompareToBuilder().append(this.sectionCode, other.sectionCode)
					.append(this.typeCode, other.typeCode).toComparison();
		}

	}

	public SectionType getSectionType() {
		return sectionType;
	}

	public void setSectionType(SectionType sectionType) {
		this.sectionType = sectionType;
	}

	public ServiceRequestType getServiceRequestType() {
		return serviceRequestType;
	}

	public void setServiceRequestType(ServiceRequestType serviceRequestType) {
		this.serviceRequestType = serviceRequestType;
	}

	public String getTypeCode() {
		return typeCode;
	}

	public void setTypeCode(String typeCode) {
		this.typeCode = typeCode;
	}

}
