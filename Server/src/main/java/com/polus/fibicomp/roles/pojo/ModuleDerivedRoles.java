package com.polus.fibicomp.roles.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.Table;

import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "MODULE_DERIVED_ROLES")
@IdClass(ModuleDerivedRoles.ModuleDerivedRoleId.class)
public class ModuleDerivedRoles implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Id
	@Column(name = "ROLE_ID")
	private Integer roleId;

	@Column(name = "ROLE_NAME")
	private String roleName;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "AUTO_GRANT_TO_PI")
	private String autoGrantPI;

	@Column(name = "AUTO_GRANT_TO_COI")
	private String autoGrantCOI;

	@Column(name = "AUTO_GRANT_TO_MODULE_CREATOR")
	private String autoGrantModuleCreator;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getRoleId() {
		return roleId;
	}

	public void setRoleId(Integer roleId) {
		this.roleId = roleId;
	}

	public String getRoleName() {
		return roleName;
	}

	public void setRoleName(String roleName) {
		this.roleName = roleName;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

	public String getAutoGrantPI() {
		return autoGrantPI;
	}

	public void setAutoGrantPI(String autoGrantPI) {
		this.autoGrantPI = autoGrantPI;
	}

	public String getAutoGrantCOI() {
		return autoGrantCOI;
	}

	public void setAutoGrantCOI(String autoGrantCOI) {
		this.autoGrantCOI = autoGrantCOI;
	}

	public String getAutoGrantModuleCreator() {
		return autoGrantModuleCreator;
	}

	public void setAutoGrantModuleCreator(String autoGrantModuleCreator) {
		this.autoGrantModuleCreator = autoGrantModuleCreator;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public static final class ModuleDerivedRoleId implements Serializable {

		private static final long serialVersionUID = 1L;

		private Integer moduleCode;

		private Integer roleId;

		public Integer getModuleCode() {
			return moduleCode;
		}

		public void setModuleCode(Integer moduleCode) {
			this.moduleCode = moduleCode;
		}

		public Integer getRoleId() {
			return roleId;
		}

		public void setRoleId(Integer roleId) {
			this.roleId = roleId;
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("moduleCode", this.moduleCode).append("roleId", this.roleId).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final ModuleDerivedRoleId rhs = (ModuleDerivedRoleId) other;
			return new EqualsBuilder().append(this.moduleCode, rhs.moduleCode).append(this.roleId, rhs.roleId).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.moduleCode).append(this.roleId).toHashCode();
		}

		public int compareTo(ModuleDerivedRoleId other) {
			return new CompareToBuilder().append(this.moduleCode, other.roleId)
					.append(this.moduleCode, other.moduleCode).append(this.roleId, other.roleId).toComparison();
		}
	}

}
