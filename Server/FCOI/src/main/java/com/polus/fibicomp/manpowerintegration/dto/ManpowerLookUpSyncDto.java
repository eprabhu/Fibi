package com.polus.fibicomp.manpowerintegration.dto;

import java.sql.Timestamp;

import com.polus.fibicomp.manpower.pojo.ManpowerInterfaceType;

public class ManpowerLookUpSyncDto {

	private ManpowerInterfaceType manpowerInterfaceType;
	private String interfaceTypeCode;
	private Integer noOfRecords;
	private Timestamp lastSyncedOn;

	public ManpowerInterfaceType getManpowerInterfaceType() {
		return manpowerInterfaceType;
	}

	public void setManpowerInterfaceType(ManpowerInterfaceType manpowerInterfaceType) {
		this.manpowerInterfaceType = manpowerInterfaceType;
	}

	public String getInterfaceTypeCode() {
		return interfaceTypeCode;
	}

	public void setInterfaceTypeCode(String interfaceTypeCode) {
		this.interfaceTypeCode = interfaceTypeCode;
	}

	public Integer getNoOfRecords() {
		return noOfRecords;
	}

	public void setNoOfRecords(Integer noOfRecords) {
		this.noOfRecords = noOfRecords;
	}

	public Timestamp getLastSyncedOn() {
		return lastSyncedOn;
	}

	public void setLastSyncedOn(Timestamp lastSyncedOn) {
		this.lastSyncedOn = lastSyncedOn;
	}

}
