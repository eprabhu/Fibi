//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.11 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2020.11.25 at 01:02:19 PM IST 
//


package com.polus.fibicomp.manpowerintegration.dto.recruiting;

import java.math.BigDecimal;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.XMLGregorianCalendar;


/**
 * Encapsulating element containing all Merit Plan Compensation data.
 * 
 * <p>Java class for Proposed_Merit_Plan_Assignment_DataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Proposed_Merit_Plan_Assignment_DataType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="Merit_Plan_Reference" type="{urn:com.workday/bsvc}Merit_PlanObjectType" minOccurs="0"/&gt;
 *         &lt;element name="Individual_Target_Amount" minOccurs="0"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal"&gt;
 *               &lt;totalDigits value="26"/&gt;
 *               &lt;fractionDigits value="6"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="Individual_Target_Percent" minOccurs="0"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal"&gt;
 *               &lt;totalDigits value="16"/&gt;
 *               &lt;minInclusive value="0"/&gt;
 *               &lt;fractionDigits value="6"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="Guaranteed_Minimum" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/&gt;
 *         &lt;element name="Actual_End_Date" type="{http://www.w3.org/2001/XMLSchema}date" minOccurs="0"/&gt;
 *       &lt;/sequence&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Proposed_Merit_Plan_Assignment_DataType", propOrder = {
    "meritPlanReference",
    "individualTargetAmount",
    "individualTargetPercent",
    "guaranteedMinimum",
    "actualEndDate"
})
public class ProposedMeritPlanAssignmentDataType {

    @XmlElement(name = "Merit_Plan_Reference")
    protected MeritPlanObjectType meritPlanReference;
    @XmlElement(name = "Individual_Target_Amount")
    protected BigDecimal individualTargetAmount;
    @XmlElement(name = "Individual_Target_Percent")
    protected BigDecimal individualTargetPercent;
    @XmlElement(name = "Guaranteed_Minimum")
    protected Boolean guaranteedMinimum;
    @XmlElement(name = "Actual_End_Date")
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar actualEndDate;

    /**
     * Gets the value of the meritPlanReference property.
     * 
     * @return
     *     possible object is
     *     {@link MeritPlanObjectType }
     *     
     */
    public MeritPlanObjectType getMeritPlanReference() {
        return meritPlanReference;
    }

    /**
     * Sets the value of the meritPlanReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link MeritPlanObjectType }
     *     
     */
    public void setMeritPlanReference(MeritPlanObjectType value) {
        this.meritPlanReference = value;
    }

    /**
     * Gets the value of the individualTargetAmount property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getIndividualTargetAmount() {
        return individualTargetAmount;
    }

    /**
     * Sets the value of the individualTargetAmount property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setIndividualTargetAmount(BigDecimal value) {
        this.individualTargetAmount = value;
    }

    /**
     * Gets the value of the individualTargetPercent property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getIndividualTargetPercent() {
        return individualTargetPercent;
    }

    /**
     * Sets the value of the individualTargetPercent property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setIndividualTargetPercent(BigDecimal value) {
        this.individualTargetPercent = value;
    }

    /**
     * Gets the value of the guaranteedMinimum property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isGuaranteedMinimum() {
        return guaranteedMinimum;
    }

    /**
     * Sets the value of the guaranteedMinimum property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setGuaranteedMinimum(Boolean value) {
        this.guaranteedMinimum = value;
    }

    /**
     * Gets the value of the actualEndDate property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getActualEndDate() {
        return actualEndDate;
    }

    /**
     * Sets the value of the actualEndDate property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setActualEndDate(XMLGregorianCalendar value) {
        this.actualEndDate = value;
    }

}
