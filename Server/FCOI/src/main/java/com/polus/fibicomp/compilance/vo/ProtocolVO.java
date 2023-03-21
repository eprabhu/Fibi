package com.polus.fibicomp.compilance.vo;

import java.sql.Timestamp;
import java.util.List;

import com.polus.fibicomp.compilance.pojo.AcProtocol;
import com.polus.fibicomp.compilance.pojo.IrbProtocol;

public class ProtocolVO {

	private String protocolNumber;

	private String protocolStatusCode;

	private String title;

	private String personId;

	private Timestamp expirationDate;

	private Boolean isEmployeeFlag = Boolean.FALSE;

	private String fundingSourceTypeCode;

	private String specialReviewTypeCode;

	private List<IrbProtocol> irbProtocols;

	private List<AcProtocol> acProtocols;

	private IrbProtocol irbProtocol;

	private AcProtocol acProtocol;

	public String getProtocolNumber() {
		return protocolNumber;
	}

	public void setProtocolNumber(String protocolNumber) {
		this.protocolNumber = protocolNumber;
	}

	public String getProtocolStatusCode() {
		return protocolStatusCode;
	}

	public void setProtocolStatusCode(String protocolStatusCode) {
		this.protocolStatusCode = protocolStatusCode;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Timestamp getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(Timestamp expirationDate) {
		this.expirationDate = expirationDate;
	}

	public Boolean getIsEmployeeFlag() {
		return isEmployeeFlag;
	}

	public void setIsEmployeeFlag(Boolean isEmployeeFlag) {
		this.isEmployeeFlag = isEmployeeFlag;
	}

	public String getFundingSourceTypeCode() {
		return fundingSourceTypeCode;
	}

	public void setFundingSourceTypeCode(String fundingSourceTypeCode) {
		this.fundingSourceTypeCode = fundingSourceTypeCode;
	}

	public String getSpecialReviewTypeCode() {
		return specialReviewTypeCode;
	}

	public void setSpecialReviewTypeCode(String specialReviewTypeCode) {
		this.specialReviewTypeCode = specialReviewTypeCode;
	}

	public List<IrbProtocol> getIrbProtocols() {
		return irbProtocols;
	}

	public void setIrbProtocols(List<IrbProtocol> irbProtocols) {
		this.irbProtocols = irbProtocols;
	}

	public List<AcProtocol> getAcProtocols() {
		return acProtocols;
	}

	public void setAcProtocols(List<AcProtocol> acProtocols) {
		this.acProtocols = acProtocols;
	}

	public IrbProtocol getIrbProtocol() {
		return irbProtocol;
	}

	public void setIrbProtocol(IrbProtocol irbProtocol) {
		this.irbProtocol = irbProtocol;
	}

	public AcProtocol getAcProtocol() {
		return acProtocol;
	}

	public void setAcProtocol(AcProtocol acProtocol) {
		this.acProtocol = acProtocol;
	}

}
