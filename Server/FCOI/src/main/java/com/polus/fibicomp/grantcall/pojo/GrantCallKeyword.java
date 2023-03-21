package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
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
import com.polus.fibicomp.pojo.ScienceKeyword;

@Entity
@Table(name = "GRANT_CALL_KEYWORDS")
public class GrantCallKeyword implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "GRANT_KEYWORD_ID", updatable = false, nullable = false)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_KEYWORD_ID_GENERATOR")
	@SequenceGenerator(name="GRANT_KEYWORD_ID_GENERATOR", sequenceName = "GRANT_KEYWORD_ID_GENERATOR", allocationSize=1)
	private Integer grantKeywordId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_KEYWORDS_FK1"), name = "GRANT_HEADER_ID", referencedColumnName = "GRANT_HEADER_ID")
	private GrantCall grantCall;

	@Column(name = "SCIENCE_KEYWORD_CODE")
	private String scienceKeywordCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "GRANT_CALL_KEYWORDS_FK2"), name = "SCIENCE_KEYWORD_CODE", referencedColumnName = "SCIENCE_KEYWORD_CODE", insertable = false, updatable = false)
	private ScienceKeyword scienceKeyword;
	
	@Column(name = "KEYWORD")
	private String keyword;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getGrantKeywordId() {
		return grantKeywordId;
	}

	public void setGrantKeywordId(Integer grantKeywordId) {
		this.grantKeywordId = grantKeywordId;
	}

	public GrantCall getGrantCall() {
		return grantCall;
	}

	public void setGrantCall(GrantCall grantCall) {
		this.grantCall = grantCall;
	}

	public String getScienceKeywordCode() {
		return scienceKeywordCode;
	}

	public void setScienceKeywordCode(String scienceKeywordCode) {
		this.scienceKeywordCode = scienceKeywordCode;
	}

	public ScienceKeyword getScienceKeyword() {
		return scienceKeyword;
	}

	public void setScienceKeyword(ScienceKeyword scienceKeyword) {
		this.scienceKeyword = scienceKeyword;
	}

	public String getKeyword() {
		return keyword;
	}

	public void setKeyword(String keyword) {
		this.keyword = keyword;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}
}
