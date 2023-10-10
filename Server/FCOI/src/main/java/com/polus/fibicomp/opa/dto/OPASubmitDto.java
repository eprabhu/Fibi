package com.polus.fibicomp.opa.dto;

public class OPASubmitDto {

    private Integer opaDisclosureId;
    private String opaDisclosureNumber;
    private String certificationText;

    public Integer getOpaDisclosureId() {
        return opaDisclosureId;
    }

    public void setOpaDisclosureId(Integer opaDisclosureId) {
        this.opaDisclosureId = opaDisclosureId;
    }

    public String getOpaDisclosureNumber() {
        return opaDisclosureNumber;
    }

    public void setOpaDisclosureNumber(String opaDisclosureNumber) {
        this.opaDisclosureNumber = opaDisclosureNumber;
    }

    public String getCertificationText() {
        return certificationText;
    }

    public void setCertificationText(String certificationText) {
        this.certificationText = certificationText;
    }
}
