package com.polus.fibicomp.coi.dto;

import java.sql.Timestamp;

public class CoiTravelDisclosureCertifyDto {

	private String certifiedBy;
	private Timestamp certifiedAt;

	public String getCertifiedBy() {
		return certifiedBy;
	}

	public void setCertifiedBy(String certifiedBy) {
		this.certifiedBy = certifiedBy;
	}

	public Timestamp getCertifiedAt() {
		return certifiedAt;
	}

	public void setCertifiedAt(Timestamp certifiedAt) {
		this.certifiedAt = certifiedAt;
	}

}
