import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ManpowerLookupComponent } from './manpower-lookup.component';
import { RouterModule } from '@angular/router';
import { SharedModule } from '../../../../shared/shared.module';
import { FormsModule } from '@angular/forms';

@NgModule({
  imports: [
    CommonModule,
    SharedModule,
    FormsModule,
    RouterModule.forChild([{path: '', component: ManpowerLookupComponent}])
  ],
  declarations: [ManpowerLookupComponent]
})
export class ManpowerLookupModule { }
