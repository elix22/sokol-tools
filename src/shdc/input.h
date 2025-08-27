#pragma once
#include <string>
#include <vector>
#include <map>
#include "types/errmsg.h"
#include "types/line.h"
#include "types/snippet.h"
#include "types/program.h"

namespace shdc {

// pre-parsed GLSL source file, with content split into snippets
struct Input {
    ErrMsg out_error;
    std::string base_path;              // path to base file
    std::string module;                 // optional module name
    std::vector<std::string> filenames; // all source files, base is first entry
    std::vector<Line> lines;          // input source files split into lines
    std::vector<Snippet> snippets;    // @block, @vs and @fs snippets
    std::map<std::string, std::string> ctype_map;    // @ctype uniform type definitions
    std::vector<std::string> headers;       // @header statements
    std::map<std::string, int> snippet_map; // name-index mapping for all code snippets
    std::map<std::string, int> block_map;   // name-index mapping for @block snippets
    std::map<std::string, int> vs_map;      // name-index mapping for @vs snippets
    std::map<std::string, int> fs_map;      // name-index mapping for @fs snippets
    std::map<std::string, int> cs_map;      // name-index mapping for @cs snippets
    std::map<std::string, Program> programs;    // all @program definitions
    std::map<std::string, ImageSampleTypeTag> image_sample_type_tags;
    std::map<std::string, SamplerTypeTag> sampler_type_tags;

    static Input load_and_parse(const std::string& path, const std::string& module_override);
    ErrMsg error(int line_index, const std::string& msg) const;
    ErrMsg warning(int line_index, const std::string& msg) const;
    void dump_debug(ErrMsg::Format err_fmt) const;
    // return nullptr if not found
    const ImageSampleTypeTag* find_image_sample_type_tag(const std::string& tex_name) const;
    const SamplerTypeTag* find_sampler_type_tag(const std::string& smp_name) const;
};

} // namespace shdc
