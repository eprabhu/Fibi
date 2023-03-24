package com.polus.fibicomp.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.adminportal.pojo.UnitAdministratorType;

@Entity
@Table(name = "UNIT_ADMINISTRATOR")
@IdClass(UnitAdministrator.UnitAdministratorId.class)
public class UnitAdministrator implements Serializable {

	private static final long serialVersionUID = 1L;
	private static final Logger LOG = LogManager.getLogger(UnitAdministrator.class);

	@Id
	@Column(name = "PERSON_ID")
	private String personId;

	@Transient
	private String fullName;

	@Transient
	private String oldPersonId;

	@Transient
	private String oldUnitAdministratorTypeCode;

	@Id
	@Column(name = "UNIT_ADMINISTRATOR_TYPE_CODE")
	private String unitAdministratorTypeCode;

	@Id
	@Column(name = "UNIT_NUMBER")
	private String unitNumber;
	
	@Transient
	private String unitName;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

//	@JsonManagedReference
//    @ManyToOne(optional = false, cascade = { CascadeType.REFRESH })
	@JsonBackReference
	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "UNIT_ADMINISTRATOR_FK1"), name = "UNIT_NUMBER", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "UNIT_ADMINISTRATOR_FK2"), name = "UNIT_ADMINISTRATOR_TYPE_CODE", referencedColumnName = "UNIT_ADMINISTRATOR_TYPE_CODE", insertable = false, updatable = false)
	private UnitAdministratorType unitAdministratorType;

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getUnitAdministratorTypeCode() {
		return unitAdministratorTypeCode;
	}

	public void setUnitAdministratorTypeCode(String unitAdministratorTypeCode) {
		this.unitAdministratorTypeCode = unitAdministratorTypeCode;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public UnitAdministratorType getUnitAdministratorType() {
		return unitAdministratorType;
	}

	public void setUnitAdministratorType(UnitAdministratorType unitAdministratorType) {
		this.unitAdministratorType = unitAdministratorType;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}
	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public static Logger getLog() {
		return LOG;
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

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getOldPersonId() {
		return oldPersonId;
	}

	public void setOldPersonId(String oldPersonId) {
		this.oldPersonId = oldPersonId;
	}

	public String getOldUnitAdministratorTypeCode() {
		return oldUnitAdministratorTypeCode;
	}

	public void setOldUnitAdministratorTypeCode(String oldUnitAdministratorTypeCode) {
		this.oldUnitAdministratorTypeCode = oldUnitAdministratorTypeCode;
	}

	public static final class UnitAdministratorId implements Serializable {

		private static final long serialVersionUID = 1L;

		private String unitNumber;

		private String personId;

		private String unitAdministratorTypeCode;

		public String getUnitNumber() {
			return this.unitNumber;
		}

		public void setUnitNumber(String unitNumber) {
			this.unitNumber = unitNumber;
		}

		public String getPersonId() {
			return this.personId;
		}

		public void setPersonId(String personId) {
			this.personId = personId;
		}

		public String getUnitAdministratorTypeCode() {
			return this.unitAdministratorTypeCode;
		}

		public void setUnitAdministratorTypeCode(String unitAdministratorTypeCode) {
			this.unitAdministratorTypeCode = unitAdministratorTypeCode;
		}
		

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("unitNumber", this.unitNumber).append("personId", this.personId)
					.append("unitAdministratorTypeCode", this.unitAdministratorTypeCode).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final UnitAdministratorId rhs = (UnitAdministratorId) other;
			return new EqualsBuilder().append(this.unitNumber, rhs.unitNumber).append(this.personId, rhs.personId)
					.append(this.unitAdministratorTypeCode, rhs.unitAdministratorTypeCode).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.unitNumber).append(this.personId)
					.append(this.unitAdministratorTypeCode).toHashCode();
		}

		public int compareTo(UnitAdministratorId other) {
			return new CompareToBuilder().append(this.unitNumber, other.unitNumber)
					.append(this.personId, other.personId)
					.append(this.unitAdministratorTypeCode, other.unitAdministratorTypeCode).toComparison();
		}
	}

}
