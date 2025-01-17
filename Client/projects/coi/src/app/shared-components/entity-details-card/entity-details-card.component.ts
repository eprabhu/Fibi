import { Component, Input, OnChanges } from '@angular/core';
import { isEmptyObject } from '../../../../../fibi/src/app/common/utilities/custom-utilities';

class CardDetails {
    entityName: string = '';
    entityType: string = '';
    countryName: string = '';
    entityStatus: string = '';
    relationship: any = [];
}
@Component({
    selector: 'app-entity-details-card',
    templateUrl: './entity-details-card.component.html',
    styleUrls: ['./entity-details-card.component.scss']
})
export class EntityDetailsCardComponent implements OnChanges {

    @Input() entityDetails: any;
    @Input() relationshipDetails: any = [];

    cardDetails: CardDetails = new CardDetails();

    ngOnChanges() {
        this.setEntityCardDetails();
    }

    setEntityCardDetails() {
        this.cardDetails.entityName = this.entityDetails.entityName;
        this.cardDetails.entityType = (this.entityDetails && this.entityDetails.entityType) ? this.entityDetails.entityType.description : '';
        this.cardDetails.countryName = (this.entityDetails && this.entityDetails.country) ? this.entityDetails.country.countryName : '';
        this.cardDetails.entityStatus = this.entityDetails ? this.entityDetails.isActive ? 'Active' : 'Inactive' : '';
        if (this.checkForRelationShipDetails()) {
            this.cardDetails.relationship = (this.groupBy(this.relationshipDetails, "coiDisclosureType", "description"));
        }
    }

    groupBy(jsonData: any, key: string, innerKey: string): any {
        return jsonData.reduce((relationsTypeGroup, item) => {
            (relationsTypeGroup[item[key][innerKey]] = relationsTypeGroup[item[key][innerKey]] || []).push(item);
            return relationsTypeGroup;
        }, {});
    }

    checkForRelationShipDetails() {
        return this.relationshipDetails && this.relationshipDetails.length > 0;
    }

    getIcon(key: string): string {
        switch (key) {
            case 'Commitment': return 'handshake';
            case 'Travel': return 'flight';
            case 'Financial': return 'paid';
            case 'Consulting' : return 'supervisor_account';
            default: return;
        }
    }

}
