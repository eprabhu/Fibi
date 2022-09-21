package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "GRANT_CALL_IOI_MEMBERS")
public class GrantCallIOIMembers implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "IOI_MEMBER_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_CALL_IOI_MEMBER_ID_GENERATOR")
	@SequenceGenerator(name="GRANT_CALL_IOI_MEMBER_ID_GENERATOR", sequenceName = "GRANT_CALL_IOI_MEMBER_ID_GENERATOR", allocationSize=1)
	private Integer grantIOIMemberId;

	@Column(name = "IOI_ID")
	private Integer grantCallIOIId;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_IOI_MEMBERS_FK"), name = "IOI_ID", referencedColumnName = "IOI_ID", insertable = false, updatable = false)
	private GrantCallIOIHeader grantCallIOIHeader;

	@Column(name = "IS_NON_EMPLOYEE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isNonEmployee = false;

	@Column(name = "MEMBER_ID")
	private String memberId;

	@Column(name = "MEMBER_NAME")
	private String memberName;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "MEMBER_ROLE")
	private Integer memberRoleId;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_IOI_MEMBERS_FK2"), name = "MEMBER_ROLE", referencedColumnName = "PROP_PERSON_ROLE_ID", insertable = false, updatable = false)
	private ProposalPersonRole propPersonRole;

	public Integer getGrantIOIMemberId() {
		return grantIOIMemberId;
	}

	public void setGrantIOIMemberId(Integer grantIOIMemberId) {
		this.grantIOIMemberId = grantIOIMemberId;
	}

	public Integer getGrantCallIOIId() {
		return grantCallIOIId;
	}

	public void setGrantCallIOIId(Integer grantCallIOIId) {
		this.grantCallIOIId = grantCallIOIId;
	}

	public GrantCallIOIHeader getGrantCallIOIHeader() {
		return grantCallIOIHeader;
	}

	public void setGrantCallIOIHeader(GrantCallIOIHeader grantCallIOIHeader) {
		this.grantCallIOIHeader = grantCallIOIHeader;
	}

	public Boolean getIsNonEmployee() {
		return isNonEmployee;
	}

	public void setIsNonEmployee(Boolean isNonEmployee) {
		this.isNonEmployee = isNonEmployee;
	}

	public String getMemberId() {
		return memberId;
	}

	public void setMemberId(String memberId) {
		this.memberId = memberId;
	}

	public String getMemberName() {
		return memberName;
	}

	public void setMemberName(String memberName) {
		this.memberName = memberName;
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

	public Integer getMemberRoleId() {
		return memberRoleId;
	}

	public void setMemberRoleId(Integer memberRoleId) {
		this.memberRoleId = memberRoleId;
	}

	public ProposalPersonRole getPropPersonRole() {
		return propPersonRole;
	}

	public void setPropPersonRole(ProposalPersonRole propPersonRole) {
		this.propPersonRole = propPersonRole;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

}
