/*
    Code for reflection parsing.
*/
#include "reflection.h"
#include "spirvcross.h"
#include "types/reflection/bindings.h"

// workaround for Compiler.comparison_ids being protected
class UnprotectedCompiler: spirv_cross::Compiler {
public:
    bool is_comparison_sampler(const spirv_cross::SPIRType &type, uint32_t id) {
        if (type.basetype == spirv_cross::SPIRType::Sampler) {
            return comparison_ids.count(id) > 0;
        }
        return 0;
    }
    bool is_used_as_depth_texture(const spirv_cross::SPIRType &type, uint32_t id) {
        if (type.basetype == spirv_cross::SPIRType::Image) {
            return comparison_ids.count(id) > 0;
        }
        return 0;
    }
};

using namespace spirv_cross;

namespace shdc::refl {

static const Type::Enum bool_types[4][4] = {
    { Type::Bool,    Type::Bool2,   Type::Bool3,   Type::Bool4 },
    { Type::Invalid, Type::Invalid, Type::Invalid, Type::Invalid },
    { Type::Invalid, Type::Invalid, Type::Invalid, Type::Invalid },
    { Type::Invalid, Type::Invalid, Type::Invalid, Type::Invalid },
};
static const Type::Enum int_types[4][4] = {
    { Type::Int,     Type::Int2,    Type::Int3,    Type::Int4 },
    { Type::Invalid, Type::Invalid, Type::Invalid, Type::Invalid },
    { Type::Invalid, Type::Invalid, Type::Invalid, Type::Invalid },
    { Type::Invalid, Type::Invalid, Type::Invalid, Type::Invalid },
};
static const Type::Enum uint_types[4][4] = {
    { Type::UInt,    Type::UInt2,   Type::UInt3,   Type::UInt4 },
    { Type::Invalid, Type::Invalid, Type::Invalid, Type::Invalid },
    { Type::Invalid, Type::Invalid, Type::Invalid, Type::Invalid },
    { Type::Invalid, Type::Invalid, Type::Invalid, Type::Invalid },
};
static const Type::Enum float_types[4][4] = {
    { Type::Float,  Type::Float2, Type::Float3, Type::Float4 },
    { Type::Mat2x1, Type::Mat2x2, Type::Mat2x3, Type::Mat2x4 },
    { Type::Mat3x1, Type::Mat3x2, Type::Mat3x3, Type::Mat3x4 },
    { Type::Mat4x1, Type::Mat4x2, Type::Mat4x3, Type::Mat4x4 },
};


// check that a program's vertex shader outputs match the fragment shader inputs
// FIXME: this should also check the attribute's type
static ErrMsg validate_linking(const Input& inp, const Program& prog, const ProgramReflection& prog_refl) {
    if (prog.has_vs_fs()) {
        for (int slot = 0; slot < StageAttr::Num; slot++) {
            const StageAttr& vs_out = prog_refl.vs().outputs[slot];
            const StageAttr& fs_inp = prog_refl.fs().inputs[slot];
            // ignore snippet-name for equality check
            if (!vs_out.equals(fs_inp)) {
                return inp.error(prog.line_index,
                    fmt::format("outputs of vs '{}' don't match inputs of fs '{}' for attr #{} (vs={},fs={})\n",
                        prog_refl.vs_name(),
                        prog_refl.fs_name(),
                        slot,
                        vs_out.name,
                        fs_inp.name));
            }
        }
    }
    return ErrMsg();
}

Reflection Reflection::build(const Args& args, const Input& inp, const std::array<Spirvcross,Slang::Num>& spirvcross_array) {
    Reflection res;
    ErrMsg err;

    // for each program, just pick the reflection info from the first compiled slang,
    // the reflection info is the same for each Slang because it has been generated
    // with the special Slang::REFLECTION, not the actual slang.
    std::vector<Bindings> prog_bindings;
    for (const auto& item: inp.programs) {
        const Program& prog = item.second;
        const Slang::Enum slang = Slang::first_valid(args.slang);
        const Spirvcross& spirvcross = spirvcross_array[slang];
        ProgramReflection prog_refl;
        prog_refl.name = prog.name;

        const SpirvcrossSource *vs_src = nullptr;
        const SpirvcrossSource *fs_src = nullptr;
        const SpirvcrossSource *cs_src = nullptr;
        if (prog.has_vs()) {
            int vs_snippet_index = inp.snippet_map.at(prog.vs_name);
            vs_src = spirvcross.find_source_by_snippet_index(vs_snippet_index);
            assert(vs_src);
            prog_refl.stages[ShaderStage::Vertex] = vs_src->stage_refl;
        }
        if (prog.has_fs()) {
            int fs_snippet_index = inp.snippet_map.at(prog.fs_name);
            fs_src = spirvcross.find_source_by_snippet_index(fs_snippet_index);
            assert(fs_src);
            prog_refl.stages[ShaderStage::Fragment] = fs_src->stage_refl;
        }
        if (prog.has_cs()) {
            int cs_snippet_index = inp.snippet_map.at(prog.cs_name);
            cs_src = spirvcross.find_source_by_snippet_index(cs_snippet_index);
            assert(cs_src);
            prog_refl.stages[ShaderStage::Compute] = cs_src->stage_refl;
        }
        if (prog.has_vs_fs()) {
            prog_refl.bindings = merge_bindings({ vs_src->stage_refl.bindings, fs_src->stage_refl.bindings }, true, err);
        } else {
            prog_refl.bindings = merge_bindings({ cs_src->stage_refl.bindings }, true, err);
        }
        if (err.valid()) {
            res.error = inp.error(prog.line_index, err.msg);
            return res;
        }
        err = validate_program_bindings(prog_refl.bindings);
        if (err.valid()) {
            res.error = inp.error(prog.line_index, err.msg);
            return res;
        }
        prog_bindings.push_back(prog_refl.bindings);

        // check that the outputs of the vertex stage match the input stage
        res.error = validate_linking(inp, prog, prog_refl);
        if (res.error.valid()) {
            return res;
        }
        res.progs.push_back(prog_refl);
    }

    // create a merged set of resource bindings across all programs
    for (const auto& prog_refl: res.progs) {
        prog_bindings.push_back(prog_refl.bindings);
    }
    res.bindings = merge_bindings(prog_bindings, false, err);
    if (err.valid()) {
        res.error = inp.error(0, err.msg);
    }
    res.sbuf_structs = merge_storagebuffer_structs(res.bindings, err);
    if (err.valid()) {
        res.error = inp.error(0, err.msg);
    }
    return res;
}

static ImageType::Enum spirtype_to_image_type(const SPIRType& type) {
    if (type.image.arrayed) {
        if (type.image.dim == spv::Dim2D) {
            return ImageType::ARRAY;
        }
    } else {
        switch (type.image.dim) {
            case spv::Dim2D:    return ImageType::_2D;
            case spv::DimCube:  return ImageType::CUBE;
            case spv::Dim3D:    return ImageType::_3D;
            default: break;
        }
    }
    // fallthrough: invalid type
    return ImageType::INVALID;
}

static StoragePixelFormat::Enum spirtype_to_storage_pixel_format(const SPIRType& type) {
    switch (type.image.format) {
        case spv::ImageFormatRgba8: return StoragePixelFormat::RGBA8;
        case spv::ImageFormatRgba8Snorm: return StoragePixelFormat::RGBA8SN;
        case spv::ImageFormatRgba8ui: return StoragePixelFormat::RGBA8UI;
        case spv::ImageFormatRgba8i: return StoragePixelFormat::RGBA8SI;
        case spv::ImageFormatRgba16ui: return StoragePixelFormat::RGBA16UI;
        case spv::ImageFormatRgba16i: return StoragePixelFormat::RGBA16SI;
        case spv::ImageFormatRgba16f: return StoragePixelFormat::RGBA16F;
        case spv::ImageFormatR32ui: return StoragePixelFormat::R32UI;
        case spv::ImageFormatR32i: return StoragePixelFormat::R32SI;
        case spv::ImageFormatR32f: return StoragePixelFormat::R32F;
        case spv::ImageFormatRg32ui: return StoragePixelFormat::RG32UI;
        case spv::ImageFormatRg32i: return StoragePixelFormat::RG32SI;
        case spv::ImageFormatRg32f: return StoragePixelFormat::RG32F;
        case spv::ImageFormatRgba32ui: return StoragePixelFormat::RGBA32UI;
        case spv::ImageFormatRgba32i: return StoragePixelFormat::RGBA32SI;
        case spv::ImageFormatRgba32f: return StoragePixelFormat::RGBA32F;
        default: return StoragePixelFormat::INVALID;
    }
}

static ImageSampleType::Enum spirtype_to_image_sample_type(const SPIRType& type) {
    if (type.image.depth) {
        return ImageSampleType::DEPTH;
    } else {
        switch (type.basetype) {
            case SPIRType::Int:
            case SPIRType::Short:
            case SPIRType::SByte:
                return ImageSampleType::SINT;
            case SPIRType::UInt:
            case SPIRType::UShort:
            case SPIRType::UByte:
                return ImageSampleType::UINT;
            default:
                return ImageSampleType::FLOAT;
        }
    }
}

static bool spirtype_to_image_multisampled(const SPIRType& type) {
    return type.image.ms;
}

Type get_type_for_attribute(const Compiler& compiler, const Resource& res_attr) {
    const SPIRType& attr_type = compiler.get_type(res_attr.type_id);
    Type out;
    out.name = res_attr.name;
    out.type = Type::Invalid;
    uint32_t col_idx = attr_type.columns - 1;
    uint32_t vec_idx = attr_type.vecsize - 1;
    if ((col_idx < 4) && (vec_idx < 4)) {
        switch (attr_type.basetype) {
            case SPIRType::Boolean:
                out.type = bool_types[col_idx][vec_idx];
                break;
            case SPIRType::Int:
                out.type = int_types[col_idx][vec_idx];
                break;
            case SPIRType::UInt:
                out.type = uint_types[col_idx][vec_idx];
                break;
            case SPIRType::Float:
                out.type = float_types[col_idx][vec_idx];
                break;
            case SPIRType::Struct:
                out.type = Type::Struct;
                break;
            default:
                break;
        }
    }
    return out;
}

StageReflection Reflection::parse_snippet_reflection(const Compiler& compiler, const Snippet& snippet, const Input& inp, const BindSlotMap& bindslot_map, ErrMsg& out_error) {
    out_error = ErrMsg();
    StageReflection refl;

    assert(snippet.index >= 0);
    refl.snippet_index = snippet.index;
    refl.snippet_name = snippet.name;

    ShaderResources shd_resources = compiler.get_shader_resources();
    // shader stage
    switch (compiler.get_execution_model()) {
        case spv::ExecutionModelVertex:   refl.stage = ShaderStage::Vertex; break;
        case spv::ExecutionModelFragment: refl.stage = ShaderStage::Fragment; break;
        case spv::ExecutionModelGLCompute: refl.stage = ShaderStage::Compute; break;
        default: refl.stage = ShaderStage::Invalid; break;
    }

    // find entry point
    const auto entry_points = compiler.get_entry_points_and_stages();
    for (const auto& item: entry_points) {
        if (compiler.get_execution_model() == item.execution_model) {
            refl.entry_point = item.name;
            break;
        }
    }

    // workgroup size
    if (refl.stage == ShaderStage::Compute) {
        for (int i = 0; i < 3; i++) {
            refl.cs_workgroup_size[i] = compiler.get_execution_mode_argument(spv::ExecutionModeLocalSize, (uint32_t)i);
        }
        // x*y*z must be a multiple of 32
        int x = refl.cs_workgroup_size[0];
        int y = refl.cs_workgroup_size[1];
        int z = refl.cs_workgroup_size[2];
        if (((x * y * z) & 31) != 0) {
            out_error = inp.error(0, "compute shader (local_size_x * local_size_y * local_size_z) must be a multiple of 32\n");
            return refl;
        }
    }

    // stage inputs and outputs
    for (const Resource& res_attr: shd_resources.stage_inputs) {
        StageAttr refl_attr;
        refl_attr.slot = compiler.get_decoration(res_attr.id, spv::DecorationLocation);
        refl_attr.name = res_attr.name;
        refl_attr.sem_name = "TEXCOORD";
        refl_attr.sem_index = refl_attr.slot;
        refl_attr.type_info = get_type_for_attribute(compiler, res_attr);

        refl.inputs[refl_attr.slot] = refl_attr;
    }
    for (const Resource& res_attr: shd_resources.stage_outputs) {
        StageAttr refl_attr;
        refl_attr.slot = compiler.get_decoration(res_attr.id, spv::DecorationLocation);
        refl_attr.name = res_attr.name;
        refl_attr.sem_name = "TEXCOORD";
        refl_attr.sem_index = refl_attr.slot;
        refl_attr.type_info = get_type_for_attribute(compiler, res_attr);

        refl.outputs[refl_attr.slot] = refl_attr;
    }
    // uniform blocks
    for (const Resource& ub_res: shd_resources.uniform_buffers) {
        assert(!ub_res.name.empty());
        UniformBlock refl_ub;
        refl_ub.sokol_slot = bindslot_map.find_uniformblock_index(ub_res.name);
        if (refl_ub.sokol_slot == -1) {
            out_error = inp.error(0, fmt::format("no binding found for uniformblock '{}' (might be unused in shader code?)\n", refl_ub.name));
            return refl;
        }
        refl_ub.name = ub_res.name;
        refl_ub.stage = refl.stage;
        refl_ub.inst_name = compiler.get_name(ub_res.id);
        if (refl_ub.inst_name.empty()) {
            refl_ub.inst_name = compiler.get_fallback_name(ub_res.id);
        }
        const BindSlot* bindslot = bindslot_map.find_uniformblock_bindslot(ub_res.name);
        assert(bindslot);
        refl_ub.hlsl_register_b_n = bindslot->hlsl.register_b_n;
        refl_ub.msl_buffer_n = bindslot->msl.buffer_n;
        refl_ub.wgsl_group0_binding_n = bindslot->wgsl.group0_binding_n;
        refl_ub.flattened = Spirvcross::can_flatten_uniform_block(compiler, ub_res);
        refl_ub.struct_info = parse_toplevel_struct(compiler, ub_res, out_error);
        if (out_error.valid()) {
            return refl;
        }
        // uniform blocks always have 16 byte alignment
        refl_ub.struct_info.align = 16;
        refl.bindings.uniform_blocks.push_back(refl_ub);
    }
    // storage buffers
    for (const Resource& sbuf_res: shd_resources.storage_buffers) {
        assert(!sbuf_res.name.empty());
        StorageBuffer refl_sbuf;
        refl_sbuf.sokol_slot = bindslot_map.find_view_index(sbuf_res.name);
        if (refl_sbuf.sokol_slot == -1) {
            out_error = inp.error(0, fmt::format("no binding found for storagebuffer '{}' (might be unused in shader code?)\n", refl_sbuf.name));
            return refl;
        }
        refl_sbuf.name = sbuf_res.name;
        refl_sbuf.inst_name = compiler.get_name(sbuf_res.id);
        if (refl_sbuf.inst_name.empty()) {
            refl_sbuf.inst_name = compiler.get_fallback_name(sbuf_res.id);
        }
        refl_sbuf.struct_info = parse_toplevel_struct(compiler, sbuf_res, out_error);
        if (out_error.valid()) {
            return refl;
        }
        // check that the size and alignment of the nested struct is identical with the outher struct
        if (refl_sbuf.struct_info.size != refl_sbuf.struct_info.struct_items[0].size) {
            out_error = inp.error(0, fmt::format("SSBO struct size doesn't match nested item struct size (in ssbo '{}')\n", refl_sbuf.name));
            return refl;
        }
        if (refl_sbuf.struct_info.align != refl_sbuf.struct_info.struct_items[0].align) {
            out_error = inp.error(0, fmt::format("SSBO struct alignment doesn't match nested item struct alignment (in ssbo '{}')\n", refl_sbuf.name));
            return refl;
        }
        const bool readonly = compiler.get_buffer_block_flags(sbuf_res.id).get(spv::DecorationNonWritable);
        refl_sbuf.stage = refl.stage;
        refl_sbuf.readonly = readonly;
        const BindSlot* bindslot = bindslot_map.find_view_bindslot(sbuf_res.name);
        assert(bindslot);
        assert(readonly == bindslot->readonly());
        refl_sbuf.hlsl_register_t_n = bindslot->hlsl.register_t_n;
        refl_sbuf.hlsl_register_u_n = bindslot->hlsl.register_u_n;
        refl_sbuf.msl_buffer_n = bindslot->msl.buffer_n;
        refl_sbuf.wgsl_group1_binding_n = bindslot->wgsl.group1_binding_n;
        refl_sbuf.glsl_binding_n = bindslot->glsl.binding_n;
        refl.bindings.storage_buffers.push_back(refl_sbuf);
    }

    // storage images
    for (const Resource& simg_res: shd_resources.storage_images) {
        assert(!simg_res.name.empty());
        const auto& spir_type = compiler.get_type(simg_res.type_id);
        const auto& mask = compiler.get_decoration_bitset(simg_res.id);
        StorageImage refl_simg;
        refl_simg.sokol_slot = bindslot_map.find_view_index(simg_res.name);
        if (refl_simg.sokol_slot == -1) {
            out_error = inp.error(0, fmt::format("no binding found for storageimage '{}' (might be unused in shader code?)\n", refl_simg.name));
            return refl;
        }
        refl_simg.name = simg_res.name;
        refl_simg.stage = refl.stage;
        const BindSlot* bindslot = bindslot_map.find_view_bindslot(simg_res.name);
        assert(bindslot);
        refl_simg.hlsl_register_u_n = bindslot->hlsl.register_u_n;
        refl_simg.msl_texture_n = bindslot->msl.texture_n;
        refl_simg.wgsl_group1_binding_n = bindslot->wgsl.group1_binding_n;
        refl_simg.glsl_binding_n = bindslot->glsl.binding_n;
        refl_simg.writeonly = mask.get(spv::DecorationNonReadable);
        assert(bindslot->writeonly() == refl_simg.writeonly);
        refl_simg.type = spirtype_to_image_type(spir_type);
        refl_simg.access_format = spirtype_to_storage_pixel_format(spir_type);
        refl.bindings.storage_images.push_back(refl_simg);
    }

    // (separate) texture images
    for (const Resource& img_res: shd_resources.separate_images) {
        assert(!img_res.name.empty());
        Texture refl_tex;
        refl_tex.sokol_slot = bindslot_map.find_view_index(img_res.name);
        if (refl_tex.sokol_slot == -1) {
            out_error = inp.error(0, fmt::format("no binding found for texture '{}' (might be unused in shader code?)\n", refl_tex.name));
            return refl;
        }
        refl_tex.name = img_res.name;
        refl_tex.stage = refl.stage;
        const BindSlot* bindslot = bindslot_map.find_view_bindslot(img_res.name);
        assert(bindslot);
        refl_tex.hlsl_register_t_n = bindslot->hlsl.register_t_n;
        refl_tex.msl_texture_n = bindslot->msl.texture_n;
        refl_tex.wgsl_group1_binding_n = bindslot->wgsl.group1_binding_n;
        const SPIRType& img_type = compiler.get_type(img_res.type_id);
        refl_tex.type = spirtype_to_image_type(img_type);
        if (((UnprotectedCompiler*)&compiler)->is_used_as_depth_texture(img_type, img_res.id)) {
            refl_tex.sample_type = ImageSampleType::DEPTH;
        } else {
            refl_tex.sample_type = spirtype_to_image_sample_type(compiler.get_type(img_type.image.type));
        }
        refl_tex.multisampled = spirtype_to_image_multisampled(img_type);
        refl.bindings.textures.push_back(refl_tex);
    }
    // (separate) samplers
    for (const Resource& smp_res: shd_resources.separate_samplers) {
        assert(!smp_res.name.empty());
        Sampler refl_smp;
        refl_smp.sokol_slot = bindslot_map.find_sampler_index(smp_res.name);
        if (refl_smp.sokol_slot == -1) {
            out_error = inp.error(0, fmt::format("no binding found for sampler '{}' (might be unused in shader code?)\n", refl_smp.name));
            return refl;
        }
        refl_smp.name = smp_res.name;
        refl_smp.stage = refl.stage;
        const BindSlot* bindslot = bindslot_map.find_sampler_bindslot(smp_res.name);
        assert(bindslot);
        refl_smp.hlsl_register_s_n = bindslot->hlsl.register_s_n;
        refl_smp.msl_sampler_n = bindslot->msl.sampler_n;
        refl_smp.wgsl_group1_binding_n = bindslot->wgsl.group1_binding_n;
        // HACK ALERT!
        const SPIRType& smp_type = compiler.get_type(smp_res.type_id);
        if (((UnprotectedCompiler*)&compiler)->is_comparison_sampler(smp_type, smp_res.id)) {
            refl_smp.type = SamplerType::COMPARISON;
        } else {
            refl_smp.type = SamplerType::FILTERING;
        }
        refl.bindings.samplers.push_back(refl_smp);
    }
    // combined texture-samplers
    for (auto& tex_smp_res: compiler.get_combined_image_samplers()) {
        TextureSampler refl_tex_smp;
        refl_tex_smp.stage = refl.stage;
        refl_tex_smp.sokol_slot = compiler.get_decoration(tex_smp_res.combined_id, spv::DecorationBinding);
        refl_tex_smp.name = compiler.get_name(tex_smp_res.combined_id);
        refl_tex_smp.texture_name = compiler.get_name(tex_smp_res.image_id);
        refl_tex_smp.sampler_name = compiler.get_name(tex_smp_res.sampler_id);
        refl.bindings.texture_samplers.push_back(refl_tex_smp);
    }
    // patch textures with overridden texture-sample-types
    for (auto& tex: refl.bindings.textures) {
        const auto* tag = inp.find_image_sample_type_tag(tex.name);
        if (tag) {
            tex.sample_type = tag->type;
        }
    }
    // patch samplers with overridden sampler-types
    for (auto& smp: refl.bindings.samplers) {
        const auto* tag = inp.find_sampler_type_tag(smp.name);
        if (tag) {
            smp.type = tag->type;
        }
    }
    return refl;
}

Bindings Reflection::merge_bindings(const std::vector<Bindings>& in_bindings, bool to_prog_bindings, ErrMsg& out_error) {
    Bindings out_bindings;
    out_error = ErrMsg();
    for (const Bindings& src_bindings: in_bindings) {

        // merge identical uniform blocks
        for (const UniformBlock& ub: src_bindings.uniform_blocks) {
            const UniformBlock* other_ub = out_bindings.find_uniform_block_by_name(ub.name);
            if (other_ub) {
                // another uniform block of the same name exists, make sure it's identical
                if (!ub.equals(*other_ub)) {
                    out_error = ErrMsg::error(fmt::format("conflicting uniform block definitions found for '{}'", ub.name));
                    return Bindings();
                }
            } else {
                out_bindings.uniform_blocks.push_back(ub);
            }
        }

        // merge identical storage buffers
        for (const StorageBuffer& sbuf: src_bindings.storage_buffers) {
            const StorageBuffer* other_sbuf = out_bindings.find_storage_buffer_by_name(sbuf.name);
            if (other_sbuf) {
                // another storage buffer of the same name exists, make sure it's identical
                if (!sbuf.equals(*other_sbuf)) {
                    out_error = ErrMsg::error(fmt::format("conflicting storage buffer definitions found for '{}'", sbuf.name));
                    return Bindings();
                }
            } else {
                out_bindings.storage_buffers.push_back(sbuf);
            }
        }

        // merge identical storage images
        for (const StorageImage& simg: src_bindings.storage_images) {
            const StorageImage* other_simg = out_bindings.find_storage_image_by_name(simg.name);
            if (other_simg) {
                // another storage image of the same name exists, make sure it's identical
                if (!simg.equals(*other_simg)) {
                    out_error = ErrMsg::error(fmt::format("conflicting storage image definitions found for '{}'", simg.name));
                    return Bindings();
                }
            } else {
                out_bindings.storage_images.push_back(simg);
            }
        }

        // merge identical textures
        for (const Texture& tex: src_bindings.textures) {
            const Texture* other_tex = out_bindings.find_texture_by_name(tex.name);
            if (other_tex) {
                // another image of the same name exists, make sure it's identical
                if (!tex.equals(*other_tex)) {
                    out_error = ErrMsg::error(fmt::format("conflicting texture definitions found for '{}'", tex.name));
                    return Bindings();
                }
            } else {
                out_bindings.textures.push_back(tex);
            }
        }

        // merge identical samplers
        for (const Sampler& smp: src_bindings.samplers) {
            const Sampler* other_smp = out_bindings.find_sampler_by_name(smp.name);
            if (other_smp) {
                // another sampler of the same name exists, make sure it's identical
                if (!smp.equals(*other_smp)) {
                    out_error = ErrMsg::error(fmt::format("conflicting sampler definitions found for '{}'", smp.name));
                    return Bindings();
                }
            } else {
                out_bindings.samplers.push_back(smp);
            }
        }

        // merge texture samplers (only for prog bindings)
        // since texture samplers will have their bindings auto-assigned their slots won't
        // match across programs, but common texture-sampler bindings across programs are also not required
        // anywhere (such bindings are only needed for generating the common slot constants)
        if (to_prog_bindings) {
            for (const TextureSampler& tex_smp: src_bindings.texture_samplers) {
                const TextureSampler* other_tex_smp = out_bindings.find_texture_sampler_by_name(tex_smp.name);
                if (other_tex_smp) {
                    // another texture sampler of the same name exists, make sure it's identical
                    if (!tex_smp.equals(*other_tex_smp)) {
                        out_error = ErrMsg::error(fmt::format("conflicting texture-sampler definition found for '{}'", tex_smp.name));
                        return Bindings();
                    }
                } else {
                    out_bindings.texture_samplers.push_back(tex_smp);
                }
            }
        }
    }

    // if requested, assign new texture-sampler slots which are unique across shader stages, this
    // is needed when merging the per-shader-stage bindings into per-program-bindings
    if (to_prog_bindings) {
        int sokol_slot = 0;
        for (TextureSampler& tex_smp: out_bindings.texture_samplers) {
            tex_smp.sokol_slot = sokol_slot++;
        }
    }

    return out_bindings;
}

const Type* Reflection::find_struct_by_typename(const std::vector<Type>& structs, const std::string& struct_typename) {
    for (const auto& t: structs) {
        if (t.struct_typename == struct_typename) {
            return &t;
        }
    }
    return nullptr;
}

std::vector<Type> Reflection::merge_storagebuffer_structs(const Bindings& bindings, ErrMsg& out_error) {
    std::vector<Type> merged_structs;
    for (const StorageBuffer& sbuf: bindings.storage_buffers) {
        const Type* other_struct = find_struct_by_typename(merged_structs, sbuf.struct_info.struct_items[0].struct_typename);
        if (other_struct) {
            // another struct of the same typename exists, make sure it's identical
            if (!sbuf.struct_info.struct_items[0].equals(*other_struct)) {
                out_error = ErrMsg::error(fmt::format("conflicting struct definitions found for '{}'", sbuf.struct_info.struct_items[0].struct_typename));
                return std::vector<Type>();
            }
        } else {
            merged_structs.push_back(sbuf.struct_info.struct_items[0]);
        }
    }
    return merged_structs;
}

Type Reflection::parse_struct_item(const Compiler& compiler, const TypeID& type_id, const TypeID& base_type_id, uint32_t item_index, ErrMsg& out_error) {
    const SPIRType& base_type = compiler.get_type(base_type_id);
    const TypeID& item_base_type_id = base_type.member_types[item_index];
    const SPIRType& item_base_type = compiler.get_type(item_base_type_id);
    const SPIRType& type = compiler.get_type(type_id);
    const TypeID& item_type_id = type.member_types[item_index];
    Type out;
    out.name = compiler.get_member_name(base_type.self, item_index);
    out.type = Type::Invalid;
    uint32_t col_idx = item_base_type.columns - 1;
    uint32_t vec_idx = item_base_type.vecsize - 1;
    if ((col_idx < 4) && (vec_idx < 4)) {
        switch (item_base_type.basetype) {
            case SPIRType::Boolean:
                out.type = bool_types[col_idx][vec_idx];
                break;
            case SPIRType::Int:
                out.type = int_types[col_idx][vec_idx];
                break;
            case SPIRType::UInt:
                out.type = uint_types[col_idx][vec_idx];
                break;
            case SPIRType::Float:
                out.type = float_types[col_idx][vec_idx];
                break;
            case SPIRType::Struct:
                out.type = Type::Struct;
                break;
            default:
                break;
        }
    }
    if (out.type == Type::Struct) {
        out.struct_typename = compiler.get_name(compiler.get_type(item_type_id).self);
    }
    if (Type::Invalid == out.type) {
        out_error = ErrMsg::error(fmt::format("struct item {} has unsupported type", out.name));
    }
    if (out.type == Type::Struct) {
        out.size = (int) compiler.get_declared_struct_size_runtime_array(item_base_type, 1);
    } else {
        out.size = (int) compiler.get_declared_struct_member_size(base_type, item_index);
        if (item_base_type.vecsize == 3) {
            out.align = 4 * 4;
        } else {
            out.align = 4 * item_base_type.vecsize;
        }
    }
    out.is_matrix = item_base_type.columns > 1;
    if (out.is_matrix) {
        out.matrix_stride = compiler.type_struct_member_matrix_stride(base_type, item_index);
    }
    out.offset = compiler.type_struct_member_offset(base_type, item_index);
    if (item_base_type.array.size() == 0) {
        out.is_array = false;
    } else if (item_base_type.array.size() == 1) {
        out.is_array = true;
        out.array_count = item_base_type.array[0];   // NOTE: may be 0 for unbounded array!
        out.array_stride = compiler.type_struct_member_array_stride(base_type, item_index);
    } else {
        out_error = ErrMsg::error(fmt::format("arrays of arrays are not supported (struct item {})", out.name));
        return out;
    }
    if (out.type == Type::Struct) {
        for (uint32_t nested_item_index = 0; nested_item_index < item_base_type.member_types.size(); nested_item_index++) {
            const Type nested_type = parse_struct_item(compiler, item_type_id, item_base_type_id, nested_item_index, out_error);
            if (out_error.valid()) {
                return out;
            }
            if (nested_type.align > out.align) {
                out.align = nested_type.align;
            }
            out.struct_items.push_back(nested_type);
        }
    }
    return out;
}

ErrMsg Reflection::validate_program_bindings(const Bindings& bindings) {
    {
        std::array<std::string, MaxUniformBlocks> ub_slots;
        for (const auto& ub: bindings.uniform_blocks) {
            const int slot = ub.sokol_slot;
            if ((slot < 0) || (slot >= MaxUniformBlocks)) {
                return ErrMsg::error(fmt::format("binding {} out of range for uniform block '{}' (must be 0..{})",
                    slot,
                    ub.name,
                    MaxUniformBlocks - 1));
            }
            if (!ub_slots[slot].empty()) {
                return ErrMsg::error(fmt::format("uniform blocks {} and {} cannot use the same binding {}",
                    ub_slots[slot],
                    ub.name,
                    slot));
            }
            ub_slots[slot] = ub.name;
        }
    }
    {
        std::array<std::string, MaxViews> view_slots;
        for (const auto& sbuf: bindings.storage_buffers) {
            const int slot = sbuf.sokol_slot;
            if ((slot < 0) || (slot >= MaxViews)) {
                return ErrMsg::error(fmt::format("binding {} out of range for resource '{}' (must be 0..{})",
                    slot,
                    sbuf.name,
                    MaxViews - 1));
            }
            if (!view_slots[slot].empty()) {
                return ErrMsg::error(fmt::format("resources '{}' and '{}' cannot use the same binding {}",
                    view_slots[slot],
                    sbuf.name,
                    slot));
            }
            view_slots[slot] = sbuf.name;
        }
        for (const auto& simg: bindings.storage_images) {
            const int slot = simg.sokol_slot;
            if ((slot < 0) || (slot >= MaxViews)) {
                return ErrMsg::error(fmt::format("binding {} out of range for resource '{}' (must be 0..{})",
                    slot,
                    simg.name,
                    MaxViews - 1));
            }
            if (!view_slots[slot].empty()) {
                return ErrMsg::error(fmt::format("resources '{}' and '{}' cannot use the same binding {}",
                    view_slots[slot],
                    simg.name,
                    slot));
            }
            view_slots[slot] = simg.name;
        }
        for (const auto& tex: bindings.textures) {
            const int slot = tex.sokol_slot;
            if ((slot < 0) || (slot > MaxViews)) {
                return ErrMsg::error(fmt::format("binding {} out of range for resource '{}' (must be 0..{})",
                    slot,
                    tex.name,
                    MaxViews - 1));
            }
            if (!view_slots[slot].empty()) {
                return ErrMsg::error(fmt::format("resources '{}' and '{}' cannot use the same binding {}",
                    view_slots[slot],
                    tex.name,
                    slot));
            }
            view_slots[slot] = tex.name;
        }
    }
    {
        std::array<std::string, MaxSamplers> smp_slots;
        for (const auto& smp: bindings.samplers) {
            const int slot = smp.sokol_slot;
            if ((slot < 0) || (slot > MaxSamplers)) {
                return ErrMsg::error(fmt::format("binding {} out of range for sampler '{}' (must be 0..{})",
                    slot,
                    smp.name,
                    MaxSamplers - 1));
            }
            if (!smp_slots[slot].empty()) {
                    return ErrMsg::error(fmt::format("samplers '{}' and '{}' cannot use the same binding {}",
                        smp_slots[slot],
                        smp.name,
                        slot));
            }
            smp_slots[slot] = smp.name;
        }
    }
    return ErrMsg();
}

Type Reflection::parse_toplevel_struct(const Compiler& compiler, const Resource& res, ErrMsg& out_error) {
    Type out;
    out.name = res.name;
    const SPIRType& struct_type = compiler.get_type(res.base_type_id);
    if (struct_type.basetype != SPIRType::Struct) {
        out_error = ErrMsg::error(fmt::format("toplevel item {} is not a struct", out.name));
        return out;
    }
    out.type = Type::Struct;
    out.struct_typename = compiler.get_name(compiler.get_type(res.type_id).self);
    out.size = (int) compiler.get_declared_struct_size_runtime_array(struct_type, 1);
    for (uint32_t item_index = 0; item_index < struct_type.member_types.size(); item_index++) {
        const Type item_type = parse_struct_item(compiler, res.type_id, res.base_type_id, item_index, out_error);
        if (out_error.valid()) {
            return out;
        }
        if (item_type.align > out.align) {
            out.align = item_type.align;
        }
        out.struct_items.push_back(item_type);
    }
    // special case for SSBOs: fix the struct size to its array stride
    if ((out.struct_items.size() == 1) && (out.struct_items[0].type == Type::Struct)) {
        if (out.struct_items[0].array_stride > out.struct_items[0].size) {
            out.struct_items[0].size = out.struct_items[0].array_stride;
        }
    }
    return out;
}

void Reflection::dump_debug(ErrMsg::Format err_fmt) const {
    const std::string indent = "  ";
    const std::string indent2 = indent + "  ";
    fmt::print(stderr, "Reflection:\n");
    if (error.valid()) {
        fmt::print(stderr, "{}error: {}\n", indent, error.as_string(err_fmt));
    } else {
        fmt::print(stderr, "{}error: not set\n", indent);
    }
    fmt::print(stderr, "{}merged bindings:\n", indent);
    bindings.dump_debug(indent2);
    fmt::print(stderr, "{}programs:\n", indent);
    for (const auto& prog: progs) {
        prog.dump_debug(indent2);
    }
}

} // namespace
