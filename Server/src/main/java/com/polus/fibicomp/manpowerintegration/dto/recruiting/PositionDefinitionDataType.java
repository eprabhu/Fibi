//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.11 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2020.11.25 at 01:02:19 PM IST 
//


package com.polus.fibicomp.manpowerintegration.dto.recruiting;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * Wrapper element for general data pertaining to a position opening.
 * 
 * <p>Java class for Position_Definition_DataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Position_Definition_DataType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="Position_ID" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="Job_Posting_Title" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="Job_Description_Summary" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/&gt;
 *         &lt;element name="Job_Description" type="{urn:com.workday/bsvc}RichText" minOccurs="0"/&gt;
 *         &lt;element name="Critical_Job" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/&gt;
 *         &lt;element name="Difficulty_to_Fill_Reference" type="{urn:com.workday/bsvc}Difficulty_to_FillObjectType" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Position_Definition_DataType", propOrder = {
    "positionID",
    "jobPostingTitle",
    "jobDescriptionSummary",
    "jobDescription",
    "criticalJob",
    "difficultyToFillReference"
})
public class PositionDefinitionDataType {

    @XmlElement(name = "Position_ID")
    protected String positionID;
    @XmlElement(name = "Job_Posting_Title")
    protected String jobPostingTitle;
    @XmlElement(name = "Job_Description_Summary")
    protected String jobDescriptionSummary;
    @XmlElement(name = "Job_Description")
    protected String jobDescription;
    @XmlElement(name = "Critical_Job")
    protected Boolean criticalJob;
    @XmlElement(name = "Difficulty_to_Fill_Reference")
    protected DifficultyToFillObjectType difficultyToFillReference;

    /**
     * Gets the value of the positionID property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPositionID() {
        return positionID;
    }

    /**
     * Sets the value of the positionID property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPositionID(String value) {
        this.positionID = value;
    }

    /**
     * Gets the value of the jobPostingTitle property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getJobPostingTitle() {
        return jobPostingTitle;
    }

    /**
     * Sets the value of the jobPostingTitle property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setJobPostingTitle(String value) {
        this.jobPostingTitle = value;
    }

    /**
     * Gets the value of the jobDescriptionSummary property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getJobDescriptionSummary() {
        return jobDescriptionSummary;
    }

    /**
     * Sets the value of the jobDescriptionSummary property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setJobDescriptionSummary(String value) {
        this.jobDescriptionSummary = value;
    }

    /**
     * Gets the value of the jobDescription property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getJobDescription() {
        return jobDescription;
    }

    /**
     * Sets the value of the jobDescription property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setJobDescription(String value) {
        this.jobDescription = value;
    }

    /**
     * Gets the value of the criticalJob property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isCriticalJob() {
        return criticalJob;
    }

    /**
     * Sets the value of the criticalJob property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setCriticalJob(Boolean value) {
        this.criticalJob = value;
    }

    /**
     * Gets the value of the difficultyToFillReference property.
     * 
     * @return
     *     possible object is
     *     {@link DifficultyToFillObjectType }
     *     
     */
    public DifficultyToFillObjectType getDifficultyToFillReference() {
        return difficultyToFillReference;
    }

    /**
     * Sets the value of the difficultyToFillReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link DifficultyToFillObjectType }
     *     
     */
    public void setDifficultyToFillReference(DifficultyToFillObjectType value) {
        this.difficultyToFillReference = value;
    }

}
