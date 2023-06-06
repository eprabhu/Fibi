import { Component, Input, OnInit } from '@angular/core';
import { Router } from '@angular/router';

class SFI_OBJECT {
  isActive = false;
  validPersonEntityRelTypes = [];
  entityType = '';
  involvementStartDate = '';
  involvementEndDate = '';
  countryName = '';
  entityId = '';
  entityName = '';
}

@Component({
  selector: 'app-shared-sfi-card',
  templateUrl: './shared-sfi-card.component.html',
  styleUrls: ['./shared-sfi-card.component.scss']
})

export class SharedSfiCardComponent implements OnInit {

  @Input() reqObject: any;
  @Input() referredFrom: 'SFI_SUMMARY' | 'ENTITIES_DASHBOARD' | 'SFI_EDIT_MODE';
  SFIObject = new SFI_OBJECT();

  constructor(private _router: Router) { }

  ngOnInit() {
    this.updateSFIObject();
  }

  private updateSFIObject(): void {
    if (this.reqObject) {
      this.SFIObject.isActive = this.isTriggeredFromDashboard() ?  this.getActiveStatus() : this.reqObject.versionStatus;
      this.SFIObject.entityId = this.isTriggeredFromDashboard() ? this.reqObject.coiFinancialEntityId : this.reqObject.personEntityId;
      this.SFIObject.entityType = this.getEntityDescription();
      this.SFIObject.countryName = this.getCountryName();
      this.SFIObject.involvementEndDate = this.reqObject.involvementEndDate;
      this.SFIObject.involvementStartDate = this.reqObject.involvementStartDate;
      this.SFIObject.validPersonEntityRelTypes = this.isTriggeredFromDashboard() ? this.reqObject.relationshipTypes : this.reqObject.validPersonEntityRelTypes;
      this.SFIObject.entityName = this.isTriggeredFromDashboard() ? this.reqObject.coiEntityName : this.getValuesFormCOIEntityObj('entityName');
    }
  }

  private getActiveStatus(): boolean {
    return this.reqObject.versionStatus === 'Active' ? true : false;
  }

  private getEntityDescription(): string|null {
    return this.isTriggeredFromDashboard() ? this.reqObject.coiEntityType
                                           : this.getValuesFormCOIEntityObj('entityType') 
                                           ? this.getValuesFormCOIEntityObj('entityType').description : null;
  }

  private getCountryName(): string|null {
    return this.isTriggeredFromDashboard() ? this.reqObject.coiEntityCountry
                                           : this.getValuesFormCOIEntityObj('country') 
                                           ? this.getValuesFormCOIEntityObj('country').countryName : null;
  }

  private isTriggeredFromDashboard(): boolean {
    return this.referredFrom === 'ENTITIES_DASHBOARD';
  }

  private getValuesFormCOIEntityObj(value): any {
    return this.reqObject.coiEntity ? this.reqObject.coiEntity[value] : null;
  }

  openSfiDetails(entityId: number, mode: string): void {
    this._router.navigate(['/coi/entity-details'], { queryParams: { entityId: entityId, mode: mode } });
  }

}
