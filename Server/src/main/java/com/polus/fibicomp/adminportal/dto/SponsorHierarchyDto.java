package com.polus.fibicomp.adminportal.dto;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.sql.Timestamp;
import java.util.List;

/**
 * SponsorHierarchyDto
 *
 * @createdBy Ajin
 * @date 24 Nov 2021
 */
public class SponsorHierarchyDto {

    private Integer sponsorGroupId;
    @NotEmpty
    private String sponsorGroupName;
    @NotNull
    private Integer sponsorOriginatingGroupId;
    @NotNull
    private Integer sponsorRootGroupId;
    private String sponsorCode;
    private SponsorDto sponsor;
    private List<SponsorHierarchyDto> childSponsorHierarchies;
    private Integer orderNumber;
    private Timestamp createTimestamp;
    private String createUser;
    private Timestamp updateTimestamp;
    private String updateUser;
    private boolean isEmptyGroup;
    private String accType;

    public SponsorHierarchyDto() {
        isEmptyGroup = false;
    }

    public Integer getSponsorGroupId() {
        return sponsorGroupId;
    }

    public void setSponsorGroupId(Integer sponsorGroupId) {
        this.sponsorGroupId = sponsorGroupId;
    }

    public String getSponsorGroupName() {
        return sponsorGroupName;
    }

    public void setSponsorGroupName(String sponsorGroupName) {
        this.sponsorGroupName = sponsorGroupName;
    }

    public Integer getSponsorOriginatingGroupId() {
        return sponsorOriginatingGroupId;
    }

    public void setSponsorOriginatingGroupId(Integer sponsorOriginatingGroupId) {
        this.sponsorOriginatingGroupId = sponsorOriginatingGroupId;
    }

    public Integer getSponsorRootGroupId() {
        return sponsorRootGroupId;
    }

    public void setSponsorRootGroupId(Integer sponsorRootGroupId) {
        this.sponsorRootGroupId = sponsorRootGroupId;
    }

    public String getSponsorCode() {
        return sponsorCode;
    }

    public void setSponsorCode(String sponsorCode) {
        this.sponsorCode = sponsorCode;
    }

    public SponsorDto getSponsor() {
        return sponsor;
    }

    public void setSponsor(SponsorDto sponsor) {
        this.sponsor = sponsor;
    }

    public List<SponsorHierarchyDto> getChildSponsorHierarchies() {
        return childSponsorHierarchies;
    }

    public void setChildSponsorHierarchies(List<SponsorHierarchyDto> childSponsorHierarchies) {
        this.childSponsorHierarchies = childSponsorHierarchies;
    }

    public Integer getOrderNumber() {
        return orderNumber;
    }

    public void setOrderNumber(Integer orderNumber) {
        this.orderNumber = orderNumber;
    }

    public Timestamp getCreateTimestamp() {
        return createTimestamp;
    }

    public void setCreateTimestamp(Timestamp createTimestamp) {
        this.createTimestamp = createTimestamp;
    }

    public String getCreateUser() {
        return createUser;
    }

    public void setCreateUser(String createUser) {
        this.createUser = createUser;
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

    public boolean isEmptyGroup() {
        return isEmptyGroup;
    }

    public void setEmptyGroup(boolean emptyGroup) {
        isEmptyGroup = emptyGroup;
    }

    public String getAccType() {
        return accType;
    }

    public void setAccType(String accType) {
        this.accType = accType;
    }
}
