package com.polus.fibicomp.pojo;

import java.io.Serializable;

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

import com.polus.fibicomp.grantcall.pojo.GrantCallType;

@Entity
@Table(name = "ACTIVITY_GRANTCALL_MAPPING")
@IdClass(ActivityGrantCallMapping.ActivityGrantCallMappingId.class)
public class ActivityGrantCallMapping implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ACTIVITY_TYPE_CODE")
	private String activityTypeCode;

	@Id
	@Column(name = "GRANT_TYPE_CODE")
	private String grantTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "ACTIVITY_GRANTCALL_MAPPING_FK1"), name = "GRANT_TYPE_CODE", referencedColumnName = "GRANT_TYPE_CODE", insertable = false, updatable = false)
	private GrantCallType grantCallType;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private String updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getActivityTypeCode() {
		return activityTypeCode;
	}

	public void setActivityTypeCode(String activityTypeCode) {
		this.activityTypeCode = activityTypeCode;
	}

	public String getGrantTypeCode() {
		return grantTypeCode;
	}

	public void setGrantTypeCode(String grantTypeCode) {
		this.grantTypeCode = grantTypeCode;
	}

	public GrantCallType getGrantCallType() {
		return grantCallType;
	}

	public void setGrantCallType(GrantCallType grantCallType) {
		this.grantCallType = grantCallType;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(String updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public static final class ActivityGrantCallMappingId implements Serializable {

		private static final long serialVersionUID = 1L;

		private String activityTypeCode;

		private String grantTypeCode;

		public String getActivityTypeCode() {
			return activityTypeCode;
		}

		public void setActivityTypeCode(String activityTypeCode) {
			this.activityTypeCode = activityTypeCode;
		}

		public String getGrantTypeCode() {
			return grantTypeCode;
		}

		public void setGrantTypeCode(String grantTypeCode) {
			this.grantTypeCode = grantTypeCode;
		}

		public static long getSerialversionuid() {
			return serialVersionUID;
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("activityTypeCode", this.activityTypeCode)
					.append("grantTypeCode", this.grantTypeCode).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final ActivityGrantCallMappingId rhs = (ActivityGrantCallMappingId) other;
			return new EqualsBuilder().append(this.activityTypeCode, rhs.activityTypeCode)
					.append(this.grantTypeCode, rhs.grantTypeCode).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.activityTypeCode).append(this.grantTypeCode).toHashCode();
		}

		public int compareTo(ActivityGrantCallMappingId other) {
			return new CompareToBuilder().append(this.activityTypeCode, other.activityTypeCode)
					.append(this.grantTypeCode, other.grantTypeCode).toComparison();
		}
	}
}
