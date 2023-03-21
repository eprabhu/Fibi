package com.polus.fibicomp.agreements.pojo;

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
@Table(name = "AGREEMENT_PLACE_HOLDER")
@IdClass(AgreementPlaceHolder.AgreementPlaceHolderId.class)
public class AgreementPlaceHolder implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PLACE_HOLDER_NAME")
	private String placeHolderName;

	@Id
	@Column(name = "PLACE_HOLDER_TYPE")
	private String placeHolderType;

	@Column(name = "VIEW_NAME")
	private String viewName;

	@Column(name = "VIEW_COLUMN")
	private String viewColumn;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private boolean isActive;

	public String getPlaceHolderName() {
		return placeHolderName;
	}

	public void setPlaceHolderName(String placeHolderName) {
		this.placeHolderName = placeHolderName;
	}

	public String getPlaceHolderType() {
		return placeHolderType;
	}

	public void setPlaceHolderType(String placeHolderType) {
		this.placeHolderType = placeHolderType;
	}

	public String getViewName() {
		return viewName;
	}

	public void setViewName(String viewName) {
		this.viewName = viewName;
	}

	public String getViewColumn() {
		return viewColumn;
	}

	public void setViewColumn(String viewColumn) {
		this.viewColumn = viewColumn;
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

	public static final class AgreementPlaceHolderId implements Serializable {

		private static final long serialVersionUID = 1L;

		private String placeHolderName;

		private String placeHolderType;

		public String getPlaceHolderName() {
			return placeHolderName;
		}

		public void setPlaceHolderName(String placeHolderName) {
			this.placeHolderName = placeHolderName;
		}

		public String getPlaceHolderType() {
			return placeHolderType;
		}

		public void setPlaceHolderType(String placeHolderType) {
			this.placeHolderType = placeHolderType;
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("placeHolderName", this.placeHolderName).append("placeHolderType", this.placeHolderType).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final AgreementPlaceHolderId rhs = (AgreementPlaceHolderId) other;
			return new EqualsBuilder().append(this.placeHolderName, rhs.placeHolderName).append(this.placeHolderType, rhs.placeHolderType).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.placeHolderName).append(this.placeHolderType).toHashCode();
		}

		public int compareTo(AgreementPlaceHolderId other) {
			return new CompareToBuilder().append(this.placeHolderName, other.placeHolderName).append(this.placeHolderType, other.placeHolderType).toComparison();
		}
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public boolean isActive() {
		return isActive;
	}

	public void setActive(boolean isActive) {
		this.isActive = isActive;
	}
}
