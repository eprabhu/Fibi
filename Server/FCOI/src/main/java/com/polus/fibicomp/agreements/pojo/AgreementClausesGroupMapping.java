package com.polus.fibicomp.agreements.pojo;

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
@Table(name = "AGREEMNT_TYPE_CLAUSES_MAPPING")
@IdClass(AgreementClausesGroupMapping.AgreementClausesGroupMappingId.class)
public class AgreementClausesGroupMapping implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AGREEMENT_TYPE_CODE")
	private String agreementTypeCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGRMNT_TYPE_CLAUSES_MAPING_FK1"), name = "AGREEMENT_TYPE_CODE", referencedColumnName = "AGREEMENT_TYPE_CODE", insertable = false, updatable = false)
	private AgreementType agreementType;

	@Id
	@Column(name = "CLAUSES_GROUP_CODE")
	private Integer clausesGroupCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGRMNT_TYPE_CLAUSES_MAPING_FK2"), name = "CLAUSES_GROUP_CODE", referencedColumnName = "CLAUSES_GROUP_CODE", insertable = false, updatable = false)
	private ClausesGroup clausesGroup;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

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

	public AgreementType getAgreementType() {
		return agreementType;
	}

	public void setAgreementType(AgreementType agreementType) {
		this.agreementType = agreementType;
	}

	public ClausesGroup getClausesGroup() {
		return clausesGroup;
	}

	public void setClausesGroup(ClausesGroup clausesGroup) {
		this.clausesGroup = clausesGroup;
	}

	public static final class AgreementClausesGroupMappingId implements Serializable{

		private static final long serialVersionUID = 1L;

		private String agreementTypeCode;

		private Integer clausesGroupCode;

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("agreemenTypeCode", this.agreementTypeCode).append("clausesGroupCode", this.clausesGroupCode).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final AgreementClausesGroupMappingId rhs = (AgreementClausesGroupMappingId) other;
			return new EqualsBuilder().append(this.agreementTypeCode, rhs.agreementTypeCode).append(this.clausesGroupCode, rhs.clausesGroupCode).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.agreementTypeCode).append(this.clausesGroupCode).toHashCode();
		}

		public int compareTo(AgreementClausesGroupMappingId other) {
			return new CompareToBuilder().append(this.agreementTypeCode, other.agreementTypeCode).append(this.clausesGroupCode, other.clausesGroupCode).toComparison();
		}
	}

	public Integer getClausesGroupCode() {
		return clausesGroupCode;
	}

	public void setClausesGroupCode(Integer clausesGroupCode) {
		this.clausesGroupCode = clausesGroupCode;
	}

	public String getAgreementTypeCode() {
		return agreementTypeCode;
	}

	public void setAgreementTypeCode(String agreementTypeCode) {
		this.agreementTypeCode = agreementTypeCode;
	}

}
