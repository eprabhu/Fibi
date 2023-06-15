import { Component, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { TravelDataStoreService } from '../../../services/travel-data-store.service';
import { TravelDisclosureService } from '../../../services/travel-disclosure.service';
import { TravelDisclosureResponseObject, TravelDisclosureTraveller } from '../../../travel-disclosure-interface';

@Component({
  selector: 'app-travel-form-summary',
  templateUrl: './travel-form-summary.component.html',
  styleUrls: ['./travel-form-summary.component.scss']
})
export class TravelFormSummaryComponent implements OnInit {

  isCollapsed = true;
  $subscriptions: Subscription[] = [];
  travelDisclosureData = new TravelDisclosureResponseObject();
  travellerTypeLookup: Array<TravelDisclosureTraveller>;

  constructor(private _dataStore: TravelDataStoreService, private _service: TravelDisclosureService) { }

  ngOnInit() {
      this.getDataFromStore();
      this.listenDataChangeFromStore();
  }

  private getDataFromStore() {
      this.travelDisclosureData = this._dataStore.getData();
      this.loadTravellerTypesLookup();
  }

  private listenDataChangeFromStore() {
      this.$subscriptions.push(
          this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
              this.getDataFromStore();
          })
      );
  }

  private loadTravellerTypesLookup(): void {
      this.$subscriptions.push(this._service.loadTravellerTypesLookup()
          .subscribe((data: any) => {
              if (data) {
                  this.travellerTypeLookup = data;
                  this.setCheckBoxValue();
              }
          }));
  }

  private setCheckBoxValue(): void {
      if (this.travelDisclosureData.travellerTypeCodeList.length > 0) {
          for (const type of this.travelDisclosureData.travellerTypeCodeList) {
              const matchingDetail = this.travellerTypeLookup.find(details => details.travelerTypeCode === type);
              if (matchingDetail) {
                  matchingDetail.isChecked = true;
              }
          }
      }
  }
}
