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
 * Encapsulating element containing all Calculated Plan Compensation data.
 * 
 * <p>Java class for Proposed_Calculated_Plan_Assignment_DataType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Proposed_Calculated_Plan_Assignment_DataType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;element name="Calculated_Plan_Reference" type="{urn:com.workday/bsvc}Calculated_Plan_InterfaceObjectType" minOccurs="0"/&gt;
 *         &lt;element name="Amount_Override" minOccurs="0"&gt;
 *           &lt;simpleType&gt;
 *             &lt;restriction base="{http://www.w3.org/2001/XMLSchema}decimal"&gt;
 *               &lt;totalDigits value="26"/&gt;
 *               &lt;fractionDigits value="6"/&gt;
 *             &lt;/restriction&gt;
 *           &lt;/simpleType&gt;
 *         &lt;/element&gt;
 *         &lt;element name="Currency_Reference" type="{urn:com.workday/bsvc}CurrencyObjectType" minOccurs="0"/&gt;
 *         &lt;element name="Frequency_Reference" type="{urn:com.workday/bsvc}FrequencyObjectType" minOccurs="0"/&gt;
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
@XmlType(name = "Proposed_Calculated_Plan_Assignment_DataType", propOrder = {
    "calculatedPlanReference",
    "amountOverride",
    "currencyReference",
    "frequencyReference",
    "actualEndDate"
})
public class ProposedCalculatedPlanAssignmentDataType {

    @XmlElement(name = "Calculated_Plan_Reference")
    protected CalculatedPlanInterfaceObjectType calculatedPlanReference;
    @XmlElement(name = "Amount_Override")
    protected BigDecimal amountOverride;
    @XmlElement(name = "Currency_Reference")
    protected CurrencyObjectType currencyReference;
    @XmlElement(name = "Frequency_Reference")
    protected FrequencyObjectType frequencyReference;
    @XmlElement(name = "Actual_End_Date")
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar actualEndDate;

    /**
     * Gets the value of the calculatedPlanReference property.
     * 
     * @return
     *     possible object is
     *     {@link CalculatedPlanInterfaceObjectType }
     *     
     */
    public CalculatedPlanInterfaceObjectType getCalculatedPlanReference() {
        return calculatedPlanReference;
    }

    /**
     * Sets the value of the calculatedPlanReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link CalculatedPlanInterfaceObjectType }
     *     
     */
    public void setCalculatedPlanReference(CalculatedPlanInterfaceObjectType value) {
        this.calculatedPlanReference = value;
    }

    /**
     * Gets the value of the amountOverride property.
     * 
     * @return
     *     possible object is
     *     {@link BigDecimal }
     *     
     */
    public BigDecimal getAmountOverride() {
        return amountOverride;
    }

    /**
     * Sets the value of the amountOverride property.
     * 
     * @param value
     *     allowed object is
     *     {@link BigDecimal }
     *     
     */
    public void setAmountOverride(BigDecimal value) {
        this.amountOverride = value;
    }

    /**
     * Gets the value of the currencyReference property.
     * 
     * @return
     *     possible object is
     *     {@link CurrencyObjectType }
     *     
     */
    public CurrencyObjectType getCurrencyReference() {
        return currencyReference;
    }

    /**
     * Sets the value of the currencyReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link CurrencyObjectType }
     *     
     */
    public void setCurrencyReference(CurrencyObjectType value) {
        this.currencyReference = value;
    }

    /**
     * Gets the value of the frequencyReference property.
     * 
     * @return
     *     possible object is
     *     {@link FrequencyObjectType }
     *     
     */
    public FrequencyObjectType getFrequencyReference() {
        return frequencyReference;
    }

    /**
     * Sets the value of the frequencyReference property.
     * 
     * @param value
     *     allowed object is
     *     {@link FrequencyObjectType }
     *     
     */
    public void setFrequencyReference(FrequencyObjectType value) {
        this.frequencyReference = value;
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
