import { Component, OnInit } from '@angular/core';
import { CoiService } from '../services/coi.service';

@Component({
  selector: 'app-history',
  templateUrl: './history.component.html',
  styleUrls: ['./history.component.scss']
})
export class HistoryComponent implements OnInit {

  coiActionLogs: any[] = [
    {
      eventType: 'Annual',
      coiDepositionStatus: {
        code: 1,
        description: 'Active'
      },
      reviewStatus: {
        code: 1,
        description: 'Active'
      },
      disclStatus: {
        code: 1,
        description: 'Active'
      },
      requestTitle: 'Revision Request',
      activeDate: '12/08/2022',
      certifiedDate: '12/08/2022',
      value: [
        {
          eventType: 'Revision',
          coiDepositionStatus: {
            code: 2,
            description: 'Pending'
          },
          reviewStatus: {
            code: 2,
            description: 'Pending'
          },
          disclStatus: {
            code: 3,
            description: 'Archive'
          },
          requestTitle: 'Revision Request',
          activeDate: '2/07/2022',
          certifiedDate: '1/07/2022'
        },
        {
          eventType: 'Revision',
          coiDepositionStatus: {
            code: 1,
            description: 'Active'
          },
          reviewStatus: {
            code: 1,
            description: 'Active'
          },
          disclStatus: {
            code: 3,
            description: 'Archive'
          },
          requestTitle: 'Revision Request',
          activeDate: '21/05/2022',
          certifiedDate: '21/05/2022'
        },
        {
          eventType: 'Revision',
          coiDepositionStatus: {
            code: 1,
            description: 'Active'
          },
          reviewStatus: {
            code: 1,
            description: 'Active'
          },
          disclStatus: {
            code: 3,
            description: 'Archive'
          },
          requestTitle: 'Revision Request',
          activeDate: '22/03/2022',
          certifiedDate: '22/03/2022'
        }
      ]
    },

  ];

  constructor(public _coiService: CoiService) { }

  ngOnInit() {
    this._coiService.isShowHistoryInfo = true;
  }

  closeHistoryInfo() {
    this._coiService.isShowHistoryInfo = false;
  }

  getBadge(code) {
    switch (code) {
      case 1: return 'success';
      case 2: return 'warning';
      case 3: return 'info';
      default: return '';
    }
  }
  getBadgeTextColor(code) {
    switch (code) {
      case 1: return 'white';
      case 2: return 'black';
      case 3: return 'white';
      default: return '';
    }
  }
}
