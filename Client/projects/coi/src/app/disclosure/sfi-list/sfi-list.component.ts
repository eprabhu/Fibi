import { Component, OnInit } from '@angular/core';
import { isEmptyObject } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { DataStoreService } from '../services/data-store.service';
import { Subscription } from 'rxjs';
import { CoiService } from '../services/coi.service';

@Component({
  selector: 'app-sfi-list',
  templateUrl: './sfi-list.component.html',
  styleUrls: ['./sfi-list.component.scss']
})
export class SfiListComponent implements OnInit {
  coiData: any;
  $subscriptions: Subscription[] = [];

  constructor(public dataStore: DataStoreService, public _coiService: CoiService) { }

  ngOnInit() {
    this.listenDataChangeFromStore();
    this.getDataFromStore();
    window.scrollTo(0, 0);
  }

  private listenDataChangeFromStore() {
    this.$subscriptions.push(
        this.dataStore.dataEvent.subscribe((dependencies: string[]) => {
            this.getDataFromStore();
        })
    );
  }

  private getDataFromStore() {
    const coiData = this.dataStore.getData();
    if (isEmptyObject(coiData)) { return; }
    this.coiData = coiData;
  }

  ngOnDestroy() {
    this._coiService.focusSFIId = null;
  }

}
