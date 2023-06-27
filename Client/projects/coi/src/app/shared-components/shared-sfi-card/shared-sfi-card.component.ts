import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { Router } from '@angular/router';
import { SharedSfiService } from './shared-sfi.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';

class SFI_OBJECT {
  isActive = 'INACTIVE';
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
  styleUrls: ['./shared-sfi-card.component.scss'],
  providers: [SharedSfiService]
})

export class SharedSfiCardComponent implements OnInit {

  @Input() reqObject: any;
  @Input() referredFrom: 'SFI_SUMMARY' | 'ENTITIES_DASHBOARD' | 'SFI_EDIT_MODE' | 'TRAVEL_DISCLOSURE';
  @Output() viewSlider = new EventEmitter<any>();
  @Output() deleteEvent =  new EventEmitter<any>();
  @Output() activateDeactivateEvent =  new EventEmitter<any>();

  SFIObject = new SFI_OBJECT();

  constructor(private _router: Router, private _sharedSFIService: SharedSfiService, private _commonService: CommonService) { }

    ngOnInit() {
      this.updateSFIObject();
    }

  private updateSFIObject(): void {
    if (this.reqObject) {
      this.SFIObject.isActive = this.isTriggeredFromDashboard() ?  this.getActiveStatus() : this.referredFrom =='SFI_EDIT_MODE' ? this.setActive() :this.reqObject.versionStatus;
      this.SFIObject.entityId = this.isTriggeredFromDashboard() ? this.reqObject.coiFinancialEntityId : this.reqObject.personEntityId;
      this.SFIObject.entityType = this.getEntityDescription();
      this.SFIObject.countryName = this.getCountryName();
      this.SFIObject.involvementEndDate = this.reqObject.involvementEndDate;
      this.SFIObject.involvementStartDate = this.reqObject.involvementStartDate;
      this.SFIObject.validPersonEntityRelTypes = this.isTriggeredFromDashboard() ? this.reqObject.relationshipTypes : this.reqObject.validPersonEntityRelTypes;
      this.SFIObject.entityName = this.isTriggeredFromDashboard() ? this.reqObject.coiEntityName : this.getValuesFormCOIEntityObj('entityName');
    }
  }

  setActive() {
    return   this.reqObject.versionStatus !== 'ACTIVE' ? 'DRAFT' : 
              this.reqObject.versionStatus === 'ACTIVE' && this.reqObject.isRelationshipActive ? 'ACTIVE' : 
              this.reqObject.versionStatus === 'ACTIVE' && !this.reqObject.isRelationshipActive ? 'INACTIVE' : '';
           
  }

  private getActiveStatus(): String {
    return this.reqObject.versionStatus === 'Active' ? 'ACTIVE' : 'INACTIVE';
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

  openSfiDetails(entityId: number, mode: string): any {
    this.viewSlider.emit({flag: true, entityId: entityId});
  }

  modifySfiDetails(entityId: number, mode: string): void {
    this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: entityId, mode: mode } });
  }

  deleteConfirmation() {
    this.deleteEvent.emit({eId: this.SFIObject.entityId});
  }

  activateDeactivate() {
    this.activateDeactivateEvent.emit(this.reqObject);
  }

}
