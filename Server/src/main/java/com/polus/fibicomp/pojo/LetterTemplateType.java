package com.polus.fibicomp.pojo;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Blob;
import java.sql.Timestamp;

@Entity
@Table(name = "LETTER_TEMPLATE_TYPE")
public class LetterTemplateType implements Serializable {

    private static final long serialVersionUID = 1L;

    @Id
    @Column(name = "LETTER_TEMPLATE_TYPE_CODE")
    private String letterTemplateTypeCode;

    @Column(name = "FILE_NAME")
    private String fileName;

    @Column(name = "CORRESPONDENCE_TEMPLATE")
    private Blob correspondenceTemplate;

    @Column(name = "CONTENT_TYPE")
    private String contentType;

    @Column(name = "MODULE_CODE")
    private Integer moduleCode;

    @Column(name = "SUB_MODULE_CODE")
    private Integer subModuleCode;

    @Column(name = "PRINT_FILE_TYPE")
    private String printFileType;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    @Column(name = "IS_ACTIVE")
    @Convert(converter = JpaCharBooleanConversion.class)
    private Boolean isActive;

    public String getLetterTemplateTypeCode() {
        return letterTemplateTypeCode;
    }

    public void setLetterTemplateTypeCode(String letterTemplateTypeCode) {
        this.letterTemplateTypeCode = letterTemplateTypeCode;
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public Blob getCorrespondenceTemplate() {
        return correspondenceTemplate;
    }

    public void setCorrespondenceTemplate(Blob correspondenceTemplate) {
        this.correspondenceTemplate = correspondenceTemplate;
    }

    public String getContentType() {
        return contentType;
    }

    public void setContentType(String contentType) {
        this.contentType = contentType;
    }

    public Integer getModuleCode() {
        return moduleCode;
    }

    public void setModuleCode(Integer moduleCode) {
        this.moduleCode = moduleCode;
    }

    public Integer getSubModuleCode() {
        return subModuleCode;
    }

    public void setSubModuleCode(Integer subModuleCode) {
        this.subModuleCode = subModuleCode;
    }

    public String getPrintFileType() {
        return printFileType;
    }

    public void setPrintFileType(String printFileType) {
        this.printFileType = printFileType;
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

    public Boolean getActive() {
        return isActive;
    }

    public void setActive(Boolean active) {
        isActive = active;
    }
}
