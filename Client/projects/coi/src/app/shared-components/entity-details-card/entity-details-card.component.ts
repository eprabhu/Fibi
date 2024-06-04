import { Component, Input, OnInit } from '@angular/core';
import { isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';

@Component({
  selector: 'app-entity-details-card',
  templateUrl: './entity-details-card.component.html',
  styleUrls: ['./entity-details-card.component.scss']
})
export class EntityDetailsCardComponent implements OnInit {

  @Input() entityDetails: any;
  @Input() relationshipDetails: any = {};

  cardDetails: any = {};
  constructor() { }

  ngOnInit() {
    this.setEntityCardDetails();
  }

  setEntityCardDetails() {
    this.cardDetails.entityName = this.entityDetails.entityName;
    this.cardDetails.entityType = (this.entityDetails && this.entityDetails.entityType) ? this.entityDetails.entityType.description : '';
    this.cardDetails.countryName = (this.entityDetails && this.entityDetails.country) ? this.entityDetails.country.countryName : '';
    this.cardDetails.entityStatus = this.entityDetails ? this.entityDetails.isActive ? 'Active' : 'Inactive' : '';
    if(this.checkForRelationShipDetails()) {
        this.cardDetails.relationship =(this.groupBy( this.relationshipDetails, "coiDisclosureType", "description"));
    }
  }

  groupBy(jsonData, key, innerKey) {
    return jsonData.reduce((relationsTypeGroup, item) => {
        (relationsTypeGroup[item[key][innerKey]] = relationsTypeGroup[item[key][innerKey]] || []).push(item);
        return relationsTypeGroup;
    }, {});
  }

  checkForRelationShipDetails() {
    return !isEmptyObject(this.relationshipDetails);
  }

  getIcon(key) {
    switch(key) {
        case 'Commitment': return 'handshake';
        case 'Travel': return 'flight';
        case 'Financial': return 'paid';
        default: return;
    }
  }

}
