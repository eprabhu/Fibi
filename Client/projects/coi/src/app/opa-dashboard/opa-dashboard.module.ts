import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { OpaDashboardComponent } from './opa-dashboard.component';
import { RouterModule, Routes } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { MatIconModule } from '@angular/material/icon';
import { EntityDetailsModule } from '../disclosure/entity-details/entity-details.module';
import { SharedComponentModule } from '../shared-components/shared-component.module';
import { SharedModule } from '../shared/shared.module';
import { OpaDashboardService } from './opa-dashboard.service';

const routes: Routes = [{path: '', component: OpaDashboardComponent}];

@NgModule({
  imports: [
        CommonModule,
        RouterModule.forChild(routes),
        SharedComponentModule,
        MatIconModule,
        FormsModule,
        SharedModule,
        EntityDetailsModule
  ],
  declarations: [OpaDashboardComponent],
  providers: [OpaDashboardService]
})
export class OpaDashboardModule { }
