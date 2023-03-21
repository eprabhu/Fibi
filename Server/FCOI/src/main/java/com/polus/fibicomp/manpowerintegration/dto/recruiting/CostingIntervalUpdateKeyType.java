//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.2.11 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2020.11.25 at 01:02:19 PM IST 
//


package com.polus.fibicomp.manpowerintegration.dto.recruiting;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSchemaType;
import javax.xml.bind.annotation.XmlType;
import javax.xml.datatype.XMLGregorianCalendar;


/**
 * Submit only when updating or deleting an existing Costing Allocation Interval.  One key (Costing Override ID or Start Date) is required.
 * 
 * <p>Java class for Costing_Interval_Update_KeyType complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="Costing_Interval_Update_KeyType"&gt;
 *   &lt;complexContent&gt;
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType"&gt;
 *       &lt;sequence&gt;
 *         &lt;choice&gt;
 *           &lt;element name="Costing_Override_ID_Update_Key" type="{http://www.w3.org/2001/XMLSchema}string"/&gt;
 *           &lt;element name="Start_Date_Update_Key" type="{http://www.w3.org/2001/XMLSchema}date"/&gt;
 *         &lt;/choice&gt;
 *       &lt;/sequence&gt;
 *       &lt;attribute name="Delete" type="{http://www.w3.org/2001/XMLSchema}boolean" /&gt;
 *     &lt;/restriction&gt;
 *   &lt;/complexContent&gt;
 * &lt;/complexType&gt;
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "Costing_Interval_Update_KeyType", propOrder = {
    "costingOverrideIDUpdateKey",
    "startDateUpdateKey"
})
public class CostingIntervalUpdateKeyType {

    @XmlElement(name = "Costing_Override_ID_Update_Key")
    protected String costingOverrideIDUpdateKey;
    @XmlElement(name = "Start_Date_Update_Key")
    @XmlSchemaType(name = "date")
    protected XMLGregorianCalendar startDateUpdateKey;
    @XmlAttribute(name = "Delete", namespace = "urn:com.workday/bsvc")
    protected Boolean delete;

    /**
     * Gets the value of the costingOverrideIDUpdateKey property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCostingOverrideIDUpdateKey() {
        return costingOverrideIDUpdateKey;
    }

    /**
     * Sets the value of the costingOverrideIDUpdateKey property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCostingOverrideIDUpdateKey(String value) {
        this.costingOverrideIDUpdateKey = value;
    }

    /**
     * Gets the value of the startDateUpdateKey property.
     * 
     * @return
     *     possible object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public XMLGregorianCalendar getStartDateUpdateKey() {
        return startDateUpdateKey;
    }

    /**
     * Sets the value of the startDateUpdateKey property.
     * 
     * @param value
     *     allowed object is
     *     {@link XMLGregorianCalendar }
     *     
     */
    public void setStartDateUpdateKey(XMLGregorianCalendar value) {
        this.startDateUpdateKey = value;
    }

    /**
     * Gets the value of the delete property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isDelete() {
        return delete;
    }

    /**
     * Sets the value of the delete property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setDelete(Boolean value) {
        this.delete = value;
    }

}
