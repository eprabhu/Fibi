package com.polus.fibicomp.opa.dto;


public class OPAAssignAdminDto {

    private Integer opaDisclosureId;
    private String opaDisclosureNumber;
    private Integer adminGroupId;
    private String adminPersonId;

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

    public Integer getAdminGroupId() {
        return adminGroupId;
    }

    public void setAdminGroupId(Integer adminGroupId) {
        this.adminGroupId = adminGroupId;
    }

    public String getAdminPersonId() {
        return adminPersonId;
    }

    public void setAdminPersonId(String adminPersonId) {
        this.adminPersonId = adminPersonId;
    }
}
