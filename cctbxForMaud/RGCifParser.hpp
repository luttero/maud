//
//  RGCifParser.hpp
//  S_FPM
//
//  Created by Luca Lutterotti on 16/06/2017.
//
//

#ifndef RGCifParser_hpp
#define RGCifParser_hpp

#include <fstream>
#include <iostream>
#include <vector>
#include <ucif/parser.h>
#include <ucif/builder.h>

namespace radiographema {
  
  namespace io {
    
    namespace cif {
      
      //@author Luca Lutterotti
      //@version 1.0
      
      class CifFile;
      
      struct cif_array_wrapper : ucif::array_wrapper_base
      {
        std::vector<std::string> array;
        
        cif_array_wrapper()
        : array()
        {}
        
        /* virtual */ void push_back(std::string const& value)
        {
          array.push_back(value);
        }
        
        /* virtual */ std::string operator[](unsigned const& i) const
        {
          return array[i];
        }
        
        /* virtual */ unsigned size() const
        {
          return array.size();
        }
      };
      
      struct cif_builder : ucif::builder_base
      {
        CifFile *cif_file = NULL;
        
        void setCifFile(CifFile *cif) { cif_file = cif; }
        
        /* virtual */ void start_save_frame(std::string const& save_frame_heading) {
//          std::cout << "save_frame_heading: " << save_frame_heading << std::endl;
        }
        
        /* virtual */ void end_save_frame() {
//          std::cout << "End save_frame: " << std::endl;
        }
        
        /* virtual */ void add_data_item(std::string const& tag, std::string const& value) {
          if (cif_file != NULL) {
            std::string key = tag;
            std::string avalue = value;
            CifItem item(key, avalue);
            cif_file->datablocks.back().items.push_back(item);
//            std::cout << "item: " << key << " = " << avalue << std::endl;
          }
        }
        
        /* virtual */ void add_loop(ucif::array_wrapper_base const& loop_headers,
                              std::vector<ucif::array_wrapper_base*> const& values) {
          if (cif_file != NULL) {
            CifLoop loop;
            for (int i = 0; i < loop_headers.size(); i++) {
              std::string key = loop_headers[i];
              loop.keys.push_back(key);
            }
            for (int i = 0; i < values.size(); i++) {
              std::vector<std::string> valuesRow =
              dynamic_cast<cif_array_wrapper*>(values[i])->array;
              for (int j = 0; j < valuesRow.size(); j++) {
                std::string value = valuesRow[j];
                CifItem item(loop.keys[i], value);
                loop.items.push_back(item);
//                std::cout << "Loop add: " << loop.keys[i] << " = " << value << std::endl;
              }
            }
            cif_file->datablocks.back().loops.push_back(loop);
          }
        }
        
        /* virtual */ void add_data_block(std::string const& data_block_heading) {
          if (cif_file != NULL) {
            CifDatablock datablock(data_block_heading.substr(5, data_block_heading.size() - 5));
            cif_file->addDatablock(datablock);
          }
        }
        
        /* virtual */ ucif::array_wrapper_base* new_array()
        {
          return new cif_array_wrapper();
        }
      };
      
    }
  }
}


#endif /* RGCifParser_hpp */
